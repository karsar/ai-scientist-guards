"""
make_disjoint_splits.py — disjoint per-hypothesis validation splits (H3).

The original create_datasets.py produced a SINGLE wine_validation.csv shared by
every hypothesis. Condition H3 (each null p-value independent of the past)
then fails two ways:

  1. every hypothesis is scored on the same validation rows, so the p-values are
     dependent through the common data; and
  2. the LLM proposes hypothesis t+1 after seeing hypothesis t's outcome
     (adaptive data analysis), so reusing one held-out set is unsound
     regardless of how the file is loaded (Dwork et al. 2015).

Disjoint pre-assigned splits fix both: hypothesis k is scored only on
validation_H{k}, rows no earlier decision ever touched, so by construction its
p-value is independent of the past. Cost: ~|pool| / n_hyp rows per hypothesis;
for data-efficient reuse instead, a reusable-holdout (Thresholdout) is the
principled alternative. This script implements the disjoint design and writes a
manifest recording the assignment.
"""

from __future__ import annotations

import json
import os

import numpy as np
import pandas as pd
from sklearn.datasets import load_wine
from sklearn.model_selection import train_test_split


def make_splits(n_hyp: int = 5, exploration_frac: float = 0.4,
                seed: int = 42, out_dir: str = "data") -> dict:
    wine = load_wine()
    df = pd.DataFrame(wine.data, columns=wine.feature_names)
    df["target"] = wine.target

    # Exploration pool (handed to optimize()) vs. validation pool (harness only).
    explore_df, valpool_df = train_test_split(
        df, test_size=(1.0 - exploration_frac), random_state=seed,
        stratify=df["target"])

    os.makedirs(out_dir, exist_ok=True)
    explore_df.to_csv(os.path.join(out_dir, "wine_exploration.csv"), index=False)

    # Partition the validation pool into n_hyp DISJOINT, stratified blocks.
    rng = np.random.default_rng(seed)
    manifest = {"n_hyp": n_hyp, "seed": seed, "splits": {}}
    assigned = {}
    for cls, grp in valpool_df.groupby("target"):
        idx = grp.index.to_numpy()
        rng.shuffle(idx)
        for k, chunk in enumerate(np.array_split(idx, n_hyp)):
            assigned.setdefault(k, []).extend(chunk.tolist())

    for k in range(n_hyp):
        rows = valpool_df.loc[assigned[k]].sort_index()
        path = os.path.join(out_dir, f"wine_validation_H{k + 1}.csv")
        rows.to_csv(path, index=False)
        manifest["splits"][f"H{k + 1}"] = {
            "path": path, "n_rows": int(rows.shape[0]),
            "row_ids": rows.index.tolist(),
        }

    # Sanity: splits are pairwise disjoint and cover the pool exactly once.
    all_ids = [i for s in manifest["splits"].values() for i in s["row_ids"]]
    assert len(all_ids) == len(set(all_ids)), "splits overlap!"
    assert set(all_ids) == set(valpool_df.index), "splits do not cover the pool"

    with open(os.path.join(out_dir, "splits_manifest.json"), "w") as f:
        json.dump(manifest, f, indent=2)
    return manifest


if __name__ == "__main__":
    m = make_splits()
    print(f"exploration + {m['n_hyp']} disjoint validation splits written to data/")
    for name, s in m["splits"].items():
        print(f"  {name}: {s['n_rows']:>3} rows  -> {s['path']}")
    print("verified: splits are pairwise disjoint and cover the validation pool.")
