"""
calibration_experiment.py — empirical demonstration of condition H1.

Under the null (no real difference between optimized and baseline models) a
valid test must produce (super-)uniform p-values. We check this directly:

  (A) Paired sign-flip permutation test on INDEPENDENT held-out per-example
      losses  -> should be super-uniform  (empirical CDF on/below diagonal).

  (B) The previous cross-validated paired t-test on CORRELATED fold scores
      (RepeatedKFold-style overlap, modelled as equicorrelated Gaussian fold
      differences) -> anti-conservative (empirical CDF above the diagonal:
      it rejects too often under the null).

The contrast shows the fix is not merely "permutation instead of t-test": it is
"independent held-out units instead of overlapping folds." Both are needed.

Run:  python calibration_experiment.py
Outputs printed summary statistics and writes calibration_pp.png if matplotlib
is available.
"""

from __future__ import annotations

import numpy as np

from verified_stats import paired_permutation_pvalue, cv_paired_ttest_pvalue


def ecdf_at(pvals: np.ndarray, grid: np.ndarray) -> np.ndarray:
    pvals = np.sort(pvals)
    return np.searchsorted(pvals, grid, side="right") / pvals.size


def ks_above_diagonal(pvals: np.ndarray) -> float:
    """Max amount by which the empirical CDF exceeds uniform (one-sided KS).
    Positive => anti-conservative (too many small p-values)."""
    p = np.sort(pvals)
    n = p.size
    return float(np.max((np.arange(1, n + 1) / n) - p))


def run_permutation_null(n_trials: int, n_examples: int, seed: int) -> np.ndarray:
    """H0: optimized and baseline have exchangeable per-example losses.
    Independent examples -> sign-flip permutation must be super-uniform."""
    rng = np.random.default_rng(seed)
    pvals = np.empty(n_trials)
    for i in range(n_trials):
        # Paired per-example losses with a shared example-difficulty component
        # (correlates A and B) but an exchangeable, independent residual. Under
        # H0 the per-example difference is symmetric about 0.
        difficulty = rng.normal(0.0, 1.0, size=n_examples)
        loss_a = difficulty + rng.normal(0.0, 1.0, size=n_examples)
        loss_b = difficulty + rng.normal(0.0, 1.0, size=n_examples)
        pvals[i] = paired_permutation_pvalue(
            loss_a, loss_b, n_perm=4000, rng=rng
        )
    return pvals


def run_cv_ttest_null(
    n_trials: int, n_folds: int, rho: float, seed: int
) -> np.ndarray:
    """H0: no difference, but fold-difference scores are positively correlated
    (equicorrelation rho), modelling RepeatedKFold's overlapping training sets.
    The paired t-test ignores this correlation -> anti-conservative."""
    rng = np.random.default_rng(seed)
    pvals = np.empty(n_trials)
    # Equicorrelated mean-zero Gaussian fold differences via a shared factor.
    shared_sd = np.sqrt(max(rho, 0.0))
    indep_sd = np.sqrt(max(1.0 - rho, 0.0))
    for i in range(n_trials):
        shared = rng.normal(0.0, shared_sd)
        diffs = shared + rng.normal(0.0, indep_sd, size=n_folds)
        # Recover paired scores with the observed differences (A = B + diff).
        scores_b = rng.normal(0.7, 0.05, size=n_folds)
        scores_a = scores_b + diffs * 0.01  # small effect scale on accuracy
        pvals[i] = cv_paired_ttest_pvalue(scores_a, scores_b)
    return pvals


def summarize(name: str, pvals: np.ndarray) -> None:
    grid = np.array([0.01, 0.05, 0.10, 0.20])
    cdf = ecdf_at(pvals, grid)
    ks = ks_above_diagonal(pvals)
    print(f"\n{name}  (n_trials={pvals.size})")
    print("  nominal alpha :   " + "  ".join(f"{a:>6.2f}" for a in grid))
    print("  empirical P(p<=a):" + "  ".join(f"{c:>6.3f}" for c in cdf))
    flags = ["OK " if c <= a + 0.02 else "HIGH" for a, c in zip(grid, cdf)]
    print("  calibration   :   " + "  ".join(f"{f:>6}" for f in flags))
    print(f"  one-sided KS above uniform: {ks:.3f} "
          f"({'super-uniform / valid' if ks < 0.03 else 'ANTI-CONSERVATIVE'})")


def main() -> None:
    n_trials = 4000
    print("=" * 64)
    print("H1 null-calibration experiment")
    print("=" * 64)

    perm = run_permutation_null(n_trials, n_examples=60, seed=1)
    summarize("(A) paired permutation test, independent held-out examples", perm)

    cvt = run_cv_ttest_null(n_trials, n_folds=30, rho=0.5, seed=2)
    summarize("(B) CV paired t-test, correlated folds (rho=0.5)", cvt)

    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt

        grid = np.linspace(0, 1, 201)
        plt.figure(figsize=(4.2, 4.2))
        plt.plot([0, 1], [0, 1], "k--", lw=1, label="uniform (target)")
        plt.plot(grid, ecdf_at(perm, grid), label="permutation (held-out)")
        plt.plot(grid, ecdf_at(cvt, grid), label="CV paired t-test")
        plt.xlabel("nominal p-value threshold u")
        plt.ylabel("empirical P(p <= u) under H0")
        plt.legend(fontsize=8, loc="lower right")
        plt.tight_layout()
        plt.savefig("calibration_pp.png", dpi=150)
        print("\nWrote calibration_pp.png")
    except Exception as e:  # pragma: no cover
        print(f"\n(plot skipped: {e})")


if __name__ == "__main__":
    main()
