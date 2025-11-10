# baseline/experiment.py
import argparse
import json
import numpy as np
import os
import sys
import pandas as pd
from sklearn.model_selection import cross_val_score, RepeatedKFold
from sklearn.svm import SVC
# REMOVED: from sklearn.preprocessing import StandardScaler
# REMOVED: from sklearn.pipeline import make_pipeline
import warnings

# === CONFIGURATION BLOCK (LLM modifies this) ===

HYPERPARAMETERS = {
    'C': 0.01,           # Very weak regularization
    'gamma': 'scale',
    'kernel': 'linear'    # Linear kernel (suboptimal for wine dataset)
}

# === END CONFIGURATION BLOCK ===

K_FOLDS = 10
N_REPEATS = 3  # Number of times to repeat K-Fold
EXPLORATION_SEED = 42
VALIDATION_SEED = 99

def build_model(config, random_state):
    """Builds the ML pipeline."""
    # DELIBERATELY WEAK: No standardization!
    model = SVC(
        C=config.get('C', 0.01),      # Low C
        gamma=config.get('gamma', 'scale'), 
        kernel=config.get('kernel', 'linear'),  # Linear kernel
        random_state=random_state
    )
    # NO PIPELINE, NO SCALER
    return model

def run_experiment(out_dir, phase):
    warnings.filterwarnings("ignore")
    
    if phase == "exploration":
        random_state = EXPLORATION_SEED
        data_file = "../data/wine_exploration.csv"
    elif phase == "validation":
        random_state = VALIDATION_SEED
        data_file = "../data/wine_validation.csv"
    else:
        raise ValueError(f"Unknown phase: {phase}")

    # 1. Load Data from CSV files
    try:
        df = pd.read_csv(data_file)
        X = df.drop('target', axis=1).values
        y = df['target'].values
    except FileNotFoundError:
        print(f"Data file not found: {data_file}")
        print("Please run create_datasets.py first to generate the data files.")
        sys.exit(1)

    # 2. Build Model (now without scaling!)
    try:
        model = build_model(HYPERPARAMETERS, random_state)

        # 3. Define RepeatedKFold strategy (instead of regular KFold)
        cv = RepeatedKFold(n_splits=K_FOLDS, n_repeats=N_REPEATS, random_state=random_state)

        # 4. Execute Cross-Validation (now returns K_FOLDS * N_REPEATS scores)
        scores = cross_val_score(model, X, y, cv=cv, scoring='accuracy', n_jobs=-1)

        # 5. Save Results
        if not os.path.exists(out_dir):
            os.makedirs(out_dir)
            
        results = {
            "status": "success",
            "hyperparameters": HYPERPARAMETERS,
            "scores": scores.tolist(),
            "mean_accuracy": np.mean(scores),
            "phase": phase,
            "seed": random_state,
            "n_scores": len(scores)  # Will be 30 instead of 10
        }
        
        with open(os.path.join(out_dir, "final_info.json"), 'w') as f:
            json.dump(results, f, indent=4)
            
        print(f"Experiment completed ({phase} phase). Mean Accuracy: {np.mean(scores)}")

    except Exception as e:
        print(f"Experiment failed: {e}")
        raise

if __name__ == '__main__':
    script_dir = os.path.dirname(os.path.abspath(__file__))
    os.chdir(script_dir)

    parser = argparse.ArgumentParser()
    parser.add_argument('--out_dir', type=str, required=True)
    parser.add_argument('--phase', type=str, required=True, choices=["exploration", "validation"])
    args = parser.parse_args()

    run_experiment(args.out_dir, args.phase)
