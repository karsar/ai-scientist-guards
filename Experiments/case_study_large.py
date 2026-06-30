"""
case_study_large.py — positive end-to-end case study on a larger dataset.

The wine case study (case_study_rerun.py) shows the valid pipeline is
conservative on small data: with ~20-70 held-out examples per hypothesis it
makes no discoveries, exposing the inflated p-values of the old CV t-test. This
script shows the complementary point on a larger dataset (digits, 1797
samples): given enough held-out data, the SAME valid pipeline (disjoint splits +
paired permutation test + LORD++) has power---it discovers genuine improvements
while correctly rejecting null hypotheses, all under a 0.05 FDR target.

Five hypotheses are tested: three are real improvements over a weak baseline,
two are nulls (models equivalent to the baseline). A correct procedure should
discover the real ones and reject the nulls.
"""

from __future__ import annotations

import numpy as np
from sklearn.base import clone
from sklearn.datasets import make_moons
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC

from case_study_rerun import gamma, lord_threshold, ALPHA, W0
from verified_stats import paired_permutation_pvalue


def load_data(seed: int):
    # Nonlinearly separable benchmark: a linear SVM cannot capture the boundary,
    # so kernel choice is decisive. Large n -> ample held-out data per split.
    return make_moons(n_samples=6000, noise=0.30, random_state=seed)


def baseline_builder():
    # Weak baseline: a linear SVM, which underfits the nonlinear boundary.
    return SVC(C=1.0, kernel="linear", random_state=0)


def hypotheses():
    """Three genuine improvements (nonlinear kernels) and two nulls (linear,
    equivalent to the baseline)."""
    return [
        ("RBF kernel", "real",
         SVC(C=1.0, kernel="rbf", gamma="scale", random_state=0)),
        ("Polynomial kernel (deg 3)", "real",
         SVC(C=1.0, kernel="poly", degree=3, random_state=0)),
        ("Null: re-fit linear baseline", "null",
         SVC(C=1.0, kernel="linear", random_state=0)),
        ("RBF, tuned C", "real",
         SVC(C=10.0, kernel="rbf", gamma="scale", random_state=0)),
        ("Null: negligible C change", "null",
         SVC(C=1.02, kernel="linear", random_state=0)),
    ]


def per_example_loss(builder, X_tr, y_tr, X_te, y_te) -> np.ndarray:
    m = clone(builder)
    m.fit(X_tr, y_tr)
    return (m.predict(X_te) != y_te).astype(float)


def disjoint_splits(X, y, n_hyp, seed):
    X_ex, X_pool, y_ex, y_pool = train_test_split(
        X, y, test_size=0.6, random_state=seed, stratify=y)
    rng = np.random.default_rng(seed)
    parts = [[] for _ in range(n_hyp)]
    for c in np.unique(y_pool):
        idx = rng.permutation(np.where(y_pool == c)[0])
        for k, chunk in enumerate(np.array_split(idx, n_hyp)):
            parts[k].extend(chunk.tolist())
    return (X_ex, y_ex), [(X_pool[p], y_pool[p]) for p in parts]


def main() -> None:
    seed = 0
    X, y = load_data(seed)
    hyps = hypotheses()
    (X_ex, y_ex), splits = disjoint_splits(X, y, len(hyps), seed)
    base = baseline_builder()

    discoveries: list[int] = []
    fp = fn = 0
    print(f"make_moons: {X.shape[0]} samples; ~{len(splits[0][1])} held-out "
          f"examples per hypothesis")
    print(f"{'t':>2}  {'Hypothesis':<26}{'truth':>6}{'perm p':>10}"
          f"{'alpha_t':>10}  Decision")
    print("-" * 70)
    for t, ((name, truth, opt), (Xv, yv)) in enumerate(zip(hyps, splits), start=1):
        lo = per_example_loss(opt, X_ex, y_ex, Xv, yv)
        lb = per_example_loss(base, X_ex, y_ex, Xv, yv)
        p = paired_permutation_pvalue(lo, lb, alternative="a_better",
                                      rng=np.random.default_rng(seed + t))
        a_t = lord_threshold(t, discoveries)
        reject = p <= a_t
        if reject:
            discoveries.append(t)
        if reject and truth == "null":
            fp += 1
        if (not reject) and truth == "real":
            fn += 1
        print(f"{t:>2}  {name:<26}{truth:>6}{p:>10.2e}{a_t:>10.5f}  "
              f"{'DISCOVERY' if reject else 'not significant'}")
    print("-" * 70)
    n_disc = len(discoveries)
    print(f"target FDR = {ALPHA}; discoveries at t = {discoveries}")
    print(f"false discoveries (nulls rejected): {fp}/{n_disc if n_disc else 0}  "
          f"-> realized FDP = {fp / max(n_disc, 1):.2f}")
    print(f"missed real effects: {fn}/3")


if __name__ == "__main__":
    main()
