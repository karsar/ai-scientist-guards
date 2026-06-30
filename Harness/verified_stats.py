"""
verified_stats.py — harness-controlled statistical tests with a guaranteed
null-calibration property.

The architecture's condition H1 asks that each null p-value be (super-)uniform:
P(p <= u) <= u for all u in [0, 1]. This is a property of the *test*, not of
who computes it. Fixing a trusted implementation of a cross-validated paired
t-test does NOT establish H1: the K-fold splits overlap, so the fold scores are
positively correlated, the paired t-statistic underestimates variance, and the
null p-values are anti-conservative (Dietterich 1998; Bengio & Grandvalet 2004,
who prove no unbiased variance estimator for K-fold CV exists).

We replace it with a paired sign-flip permutation test applied to *independent*
held-out per-example losses. Under the null "model A and model B are
exchangeable on each held-out example", the per-example differences are
sign-symmetric and independent, so the permutation p-value is super-uniform in
finite samples, distribution-free. Independence of the units is essential: the
test is valid on held-out examples (one fixed trained model, i.i.d. examples),
not on overlapping CV folds. This is why H1 and the disjoint-split design for
H3 are two halves of the same fix.
"""

from __future__ import annotations

import numpy as np


def paired_permutation_pvalue(
    loss_a,
    loss_b,
    *,
    alternative: str = "a_better",
    n_perm: int = 20000,
    exact_max_n: int = 22,
    rng: np.random.Generator | None = None,
) -> float:
    """One-sided paired sign-flip permutation test.

    Parameters
    ----------
    loss_a, loss_b : array-like, shape (n,)
        Per-example losses (lower is better) for the optimized model A and the
        baseline B, paired on the same held-out examples.
    alternative : "a_better" tests HA: A has lower loss than B.
    n_perm : Monte-Carlo sign-flip draws used when exact enumeration is too big.
    exact_max_n : enumerate all 2**n sign assignments when n <= this.

    Returns
    -------
    A super-uniform p-value in (0, 1].
    """
    d = np.asarray(loss_b, dtype=float) - np.asarray(loss_a, dtype=float)
    if d.ndim != 1:
        raise ValueError("loss_a, loss_b must be 1-D paired arrays")
    n = d.shape[0]
    if n == 0:
        return 1.0
    if alternative != "a_better":
        d = -d  # symmetric handling; statistic is sum(d)
    obs = d.sum()

    if n <= exact_max_n:
        # Exact randomization distribution over all 2**n sign vectors.
        idx = np.arange(1 << n, dtype=np.int64)
        signs = ((idx[:, None] >> np.arange(n)) & 1).astype(np.float64) * 2.0 - 1.0
        stats = signs @ d
        # The all-(+1) assignment reproduces obs and is included -> super-uniform.
        return float(np.mean(stats >= obs - 1e-12))

    # Monte-Carlo permutation p-value with the +1/+1 correction
    # (Phipson & Smyth 2010): guarantees validity / super-uniformity.
    rng = np.random.default_rng() if rng is None else rng
    count = 0
    block = 4096
    drawn = 0
    while drawn < n_perm:
        b = min(block, n_perm - drawn)
        flips = rng.integers(0, 2, size=(b, n)).astype(np.float64) * 2.0 - 1.0
        stats = flips @ d
        count += int(np.sum(stats >= obs - 1e-12))
        drawn += b
    return float((count + 1) / (n_perm + 1))


def cv_paired_ttest_pvalue(scores_a, scores_b) -> float:
    """The PREVIOUS test, kept only for the calibration comparison.

    One-sided paired t-test on cross-validation fold scores (higher is better),
    matching the original harness `execute_paired_ttest`. Retained so the
    calibration experiment can show, on identical data, that it is
    anti-conservative while the permutation test is not. NOT used in the harness.
    """
    from scipy import stats as _st

    a = np.asarray(scores_a, dtype=float)
    b = np.asarray(scores_b, dtype=float)
    if np.allclose(a, b):
        return 1.0
    t_stat, p_two = _st.ttest_rel(a, b)
    if a.mean() <= b.mean():
        return 1.0
    return float(p_two / 2.0)
