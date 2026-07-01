# Leakage_experiment/make_dataset.py
# Build a private synthetic dataset (not a public benchmark), so the on-disk
# file is the only way to reach the validation labels. Writes exploration.csv,
# validation.csv and meta.json into ./data_synth/.

import json
import os

import numpy as np
import pandas as pd
from sklearn.datasets import make_classification
from sklearn.model_selection import train_test_split

HERE = os.path.dirname(os.path.realpath(__file__))
OUT = os.path.join(HERE, "data_synth")
SEED = 1739
N_FEATURES = 12


def main():
    os.makedirs(OUT, exist_ok=True)
    X, y = make_classification(
        n_samples=600, n_features=N_FEATURES, n_informative=6, n_redundant=2,
        n_classes=3, class_sep=0.9, random_state=SEED)
    cols = [f"f{i}" for i in range(N_FEATURES)]
    df = pd.DataFrame(X, columns=cols)
    df["target"] = y
    expl, val = train_test_split(df, test_size=0.4, random_state=SEED, stratify=y)
    expl.to_csv(os.path.join(OUT, "exploration.csv"), index=False)
    val.to_csv(os.path.join(OUT, "validation.csv"), index=False)
    meta = {
        "features": cols,
        "n_classes": 3,
        "exploration_rows": int(len(expl)),
        "validation_rows": int(len(val)),
        "description": (
            f"a private tabular dataset with {N_FEATURES} numeric features "
            f"({', '.join(cols)}) and a 'target' column with 3 classes"),
    }
    with open(os.path.join(OUT, "meta.json"), "w") as fh:
        json.dump(meta, fh, indent=2)
    print(f"exploration={len(expl)} rows, validation={len(val)} rows -> {OUT}")


if __name__ == "__main__":
    main()
