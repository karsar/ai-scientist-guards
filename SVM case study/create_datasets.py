# Create the wine dataset CSVs for the case study: one shared exploration split
# and DISJOINT per-hypothesis validation splits (condition H3). Each hypothesis
# is scored only on its own split, so its p-value is independent of the others'.
import json
import os

import numpy as np
import pandas as pd
from sklearn.datasets import load_wine
from sklearn.model_selection import train_test_split

N_HYPOTHESES = 5
SEED = 42


def create_wine_csv_files(n_hyp: int = N_HYPOTHESES, seed: int = SEED) -> None:
    wine = load_wine()
    df = pd.DataFrame(wine.data, columns=wine.feature_names)
    df["target"] = wine.target

    # Exploration split (handed to optimize()) vs. a validation pool the harness
    # partitions into disjoint per-hypothesis blocks.
    explore_df, valpool_df = train_test_split(
        df, test_size=0.6, random_state=seed, stratify=df["target"])

    os.makedirs("data", exist_ok=True)
    explore_df.to_csv("data/wine_exploration.csv", index=False)

    # Stratified, disjoint partition of the validation pool into n_hyp blocks.
    rng = np.random.default_rng(seed)
    assigned: dict = {}
    for _, grp in valpool_df.groupby("target"):
        idx = grp.index.to_numpy()
        rng.shuffle(idx)
        for k, chunk in enumerate(np.array_split(idx, n_hyp)):
            assigned.setdefault(k, []).extend(chunk.tolist())

    manifest = {"n_hyp": n_hyp, "seed": seed, "splits": {}}
    for k in range(n_hyp):
        rows = valpool_df.loc[assigned[k]].sort_index()
        path = f"data/wine_validation_H{k + 1}.csv"
        rows.to_csv(path, index=False)
        manifest["splits"][f"H{k + 1}"] = {"path": path, "n_rows": int(rows.shape[0])}

    # Sanity: the splits are pairwise disjoint and cover the pool exactly once.
    all_ids = sum(assigned.values(), [])
    assert len(all_ids) == len(set(all_ids)), "validation splits overlap!"
    with open("data/splits_manifest.json", "w") as f:
        json.dump(manifest, f, indent=2)

    print(f"exploration: {explore_df.shape[0]} rows -> data/wine_exploration.csv")
    for name, s in manifest["splits"].items():
        print(f"  {name}: {s['n_rows']:>3} rows -> {s['path']}")
    print("validation splits are disjoint and cover the pool (H3).")


if __name__ == "__main__":
    create_wine_csv_files()
