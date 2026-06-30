"""
case_study_rerun.py — end-to-end SVM/wine case study under the revised harness.

Runs the five optimization hypotheses through the integrated pipeline that the
revised paper describes:
  * each hypothesis is validated on its own DISJOINT split (H3);
  * the p-value is a paired sign-flip PERMUTATION test on held-out per-example
    losses (H1, super-uniform under the null);
  * the p-values feed the LORD++ online-FDR procedure (Eq. 1), which makes the
    accept/reject decisions under a 0.05 FDR target.

Produces the execution trace that replaces Table 2 (t, hypothesis, p-value,
LORD++ threshold, decision). Deterministic given the seed.
"""

from __future__ import annotations

import math

import numpy as np
from sklearn.datasets import load_wine
from sklearn.feature_selection import RFE
from sklearn.model_selection import GridSearchCV, train_test_split
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC

from sklearn.model_selection import RepeatedKFold

from verified_stats import paired_permutation_pvalue, cv_paired_ttest_pvalue

ALPHA = 0.05          # target FDR
W0 = ALPHA / 2.0      # initial wealth, in (0, alpha)
GAMMA_C = 0.0772      # normalizing constant for the standard gamma sequence


def gamma(j: int) -> float:
    """Standard LORD++ gamma_j = c*log(max(j,2)) / (j*exp(sqrt(log j)))."""
    if j <= 0:
        return 0.0
    return GAMMA_C * math.log(max(j, 2)) / (j * math.exp(math.sqrt(math.log(max(j, 1)))))


def lord_threshold(t: int, discoveries: list[int]) -> float:
    """alpha_t = gamma_t*W0 + (alpha-W0)*sum_{tau<t} gamma_(t-tau)  (Eq. 1)."""
    term1 = gamma(t) * W0
    term2 = (ALPHA - W0) * sum(gamma(t - tau) for tau in discoveries if tau < t)
    return term1 + term2


def baseline_builder():
    return SVC(C=0.01, kernel="linear", random_state=0)


def hypothesis_builders():
    """The five optimization hypotheses from the running example."""
    return [
        ("RBF kernel optimization",
         GridSearchCV(SVC(random_state=0),
                      {"C": [1, 10], "gamma": ["scale", 0.01]}, cv=3)),
        ("Feature scaling",
         make_pipeline(StandardScaler(), SVC(C=0.01, kernel="linear", random_state=0))),
        ("C regularization",
         make_pipeline(StandardScaler(), SVC(C=10.0, kernel="linear", random_state=0))),
        ("Polynomial kernel",
         make_pipeline(StandardScaler(), SVC(C=1.0, kernel="poly", degree=3, random_state=0))),
        ("Feature selection (RFE)",
         make_pipeline(StandardScaler(),
                       RFE(SVC(C=1.0, kernel="linear", random_state=0), n_features_to_select=6))),
    ]


def per_example_loss(builder, X_tr, y_tr, X_te, y_te) -> np.ndarray:
    from sklearn.base import clone
    m = clone(builder)
    m.fit(X_tr, y_tr)
    return (m.predict(X_te) != y_te).astype(float)


def cv_ttest_p(opt, base, X, y, seed: int) -> float:
    """The OLD harness method: RepeatedKFold(10x3) fold accuracies fed to a
    one-sided paired t-test. Anti-conservative on overlapping folds."""
    from sklearn.base import clone
    rkf = RepeatedKFold(n_splits=10, n_repeats=3, random_state=seed)
    sa, sb = [], []
    for tr, te in rkf.split(X):
        a = clone(opt).fit(X[tr], y[tr]); b = clone(base).fit(X[tr], y[tr])
        sa.append((a.predict(X[te]) == y[te]).mean())
        sb.append((b.predict(X[te]) == y[te]).mean())
    return cv_paired_ttest_pvalue(sa, sb)


def make_disjoint_splits(n_hyp: int, seed: int):
    wine = load_wine()
    X, y = wine.data, wine.target
    X_ex, X_pool, y_ex, y_pool = train_test_split(
        X, y, test_size=0.6, random_state=seed, stratify=y)
    rng = np.random.default_rng(seed)
    idx_by_class = {c: rng.permutation(np.where(y_pool == c)[0]) for c in np.unique(y_pool)}
    splits = [[] for _ in range(n_hyp)]
    for c, idx in idx_by_class.items():
        for k, chunk in enumerate(np.array_split(idx, n_hyp)):
            splits[k].extend(chunk.tolist())
    return (X_ex, y_ex), [(X_pool[s], y_pool[s]) for s in splits]


def main() -> None:
    seed = 42
    hyps = hypothesis_builders()
    (X_ex, y_ex), val_splits = make_disjoint_splits(len(hyps), seed)
    base = baseline_builder()

    # Shared validation pool for the OLD (CV t-test) method, mirroring the
    # original harness's single wine_validation.csv.
    X_shared = np.vstack([Xv for Xv, _ in val_splits])
    y_shared = np.concatenate([yv for _, yv in val_splits])

    disc_cv: list[int] = []     # discoveries under the invalid CV t-test
    disc_perm: list[int] = []   # discoveries under the valid permutation test
    print(f"{'t':>2}  {'Hypothesis':<24} {'CV t-test':>10} {'perm':>8} "
          f"{'alpha_t':>9}  CV?  perm?")
    print("-" * 72)
    for t, ((name, opt), (Xv, yv)) in enumerate(zip(hyps, val_splits), start=1):
        # OLD: shared validation pool, RepeatedKFold paired t-test.
        p_cv = cv_ttest_p(opt, base, X_shared, y_shared, seed + t)
        # NEW: train on exploration, per-example losses on the DISJOINT split,
        # paired sign-flip permutation test (super-uniform null).
        loss_o = per_example_loss(opt, X_ex, y_ex, Xv, yv)
        loss_b = per_example_loss(base, X_ex, y_ex, Xv, yv)
        p_perm = paired_permutation_pvalue(loss_o, loss_b, alternative="a_better",
                                           rng=np.random.default_rng(seed + t))
        a_cv = lord_threshold(t, disc_cv)
        a_pm = lord_threshold(t, disc_perm)
        r_cv = p_cv <= a_cv
        r_pm = p_perm <= a_pm
        if r_cv:
            disc_cv.append(t)
        if r_pm:
            disc_perm.append(t)
        print(f"{t:>2}  {name:<24} {p_cv:>10.5f} {p_perm:>8.4f} {a_pm:>9.5f}  "
              f"{'YES' if r_cv else ' no':>3}  {'YES' if r_pm else ' no':>4}")
    print("-" * 72)
    print(f"target FDR = {ALPHA}; W0 = {W0}")
    print(f"CV t-test (invalid) discoveries:    t = {disc_cv}")
    print(f"permutation (valid) discoveries:    t = {disc_perm}")


if __name__ == "__main__":
    main()
