import numpy as np
import pandas as pd
from sklearn.svm import SVC
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import GridSearchCV

def get_baseline():
    """Returns the baseline model artifact."""
    # Baseline model with weak parameters
    model = make_pipeline(
        SVC(
            C=0.01,
            gamma='scale',
            kernel='linear',
            random_state=42
        )
    )
    return model

def optimize(data):
    """Optimize the model using the exploration data."""
    X = data.drop('target', axis=1).values
    y = data['target'].values

    # Define the model with a pipeline including StandardScaler and SVC
    pipeline = make_pipeline(
        StandardScaler(),
        SVC(kernel='rbf', random_state=42)
    )
    
    # Define the parameter grid for GridSearchCV
    param_grid = {
        'svc__C': [0.1, 1.0, 10.0],
        'svc__gamma': ['scale', 0.01, 0.1, 1.0]
    }
    
    # Perform grid search with cross-validation
    grid_search = GridSearchCV(
        estimator=pipeline,
        param_grid=param_grid,
        cv=5,  # 5-fold cross-validation
        scoring='accuracy',
        n_jobs=-1
    )
    
    # Fit grid search to the data
    grid_search.fit(X, y)
    
    # Return the best model found by grid search
    return grid_search.best_estimator_

def evaluate_model(artifact, X_train, y_train, X_test, y_test):
    """Evaluate the model artifact and return the accuracy score."""
    # Fit the model on the training data
    artifact.fit(X_train, y_train)
    
    # Evaluate the model on the test data
    accuracy = artifact.score(X_test, y_test)
    
    return accuracy
