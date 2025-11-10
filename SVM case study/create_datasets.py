# Script to create the wine dataset CSV files required by the harness
import pandas as pd
import numpy as np
from sklearn.datasets import load_wine
from sklearn.model_selection import train_test_split
import os

def create_wine_csv_files():
    """Create exploration and validation CSV files from the wine dataset"""
    
    # Load wine dataset (same as baseline)
    X, y = load_wine(return_X_y=True)
    
    # Get feature names
    wine_data = load_wine()
    feature_names = wine_data.feature_names
    
    # Create DataFrame with proper column names
    df = pd.DataFrame(X, columns=feature_names)
    df['target'] = y
    
    # Split into exploration (60%) and validation (40%) with stratification
    exploration_df, validation_df = train_test_split(
        df, 
        test_size=0.4, 
        random_state=42, 
        stratify=y
    )
    
    # Create data directory
    os.makedirs('data', exist_ok=True)
    
    # Save to CSV files
    exploration_df.to_csv('data/wine_exploration.csv', index=False)
    validation_df.to_csv('data/wine_validation.csv', index=False)
    
    print(f"Created exploration dataset: {exploration_df.shape} -> data/wine_exploration.csv")
    print(f"Created validation dataset: {validation_df.shape} -> data/wine_validation.csv")
    print(f"Features: {list(feature_names)}")
    print(f"Target classes: {sorted(df['target'].unique())}")

if __name__ == "__main__":
    create_wine_csv_files()

