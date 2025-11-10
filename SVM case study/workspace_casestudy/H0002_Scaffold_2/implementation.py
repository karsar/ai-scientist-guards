# implementation.py
import numpy as np
from sklearn.svm import SVC
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler, MinMaxScaler, RobustScaler
from sklearn.model_selection import GridSearchCV

# Baseline model configuration
HYPERPARAMETERS = {
    'C': 0.01,
    'gamma': 'scale',
    'kernel': 'linear'
}

def get_baseline():
    """Returns the baseline model artifact."""
    # Baseline model without scaling
    model = SVC(
        C=HYPERPARAMETERS['C'],
        gamma=HYPERPARAMETERS['gamma'],
        kernel=HYPERPARAMETERS['kernel'],
        random_state=42
    )
    return model

def optimize(data):
    """Optimize the model by testing different feature scaling methods."""
    X = data.drop('target', axis=1).values
    y = data['target'].values

    # Define the pipeline with a placeholder for the scaler
    pipe = Pipeline([
        ('scaler', StandardScaler()),  # Default scaler
        ('svc', SVC(random_state=42))
    ])

    # Define the parameter grid for GridSearchCV
    param_grid = {
        'scaler': [StandardScaler(), MinMaxScaler(), RobustScaler()],
        'svc__C': [0.1, 1.0, 10.0],
        'svc__kernel': ['rbf', 'poly'],
        'svc__degree': [2, 3],  # Only relevant for 'poly' kernel
        'svc__gamma': ['scale', 'auto']
    }

    # Set up the grid search
    grid_search = GridSearchCV(pipe, param_grid, scoring='accuracy', cv=3, n_jobs=-1)

    # Fit the grid search
    grid_search.fit(X, y)

    # Return the best model found by grid search
    return grid_search.best_estimator_

def evaluate_model(artifact, X_train, y_train, X_test, y_test):
    """Evaluate the model within a single cross-validation fold."""
    # Fit the model on the training data
    artifact.fit(X_train, y_train)

    # Evaluate the model on the test data
    accuracy = artifact.score(X_test, y_test)

    return accuracy
