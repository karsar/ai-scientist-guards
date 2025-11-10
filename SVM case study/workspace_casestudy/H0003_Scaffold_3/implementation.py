# implementation.py
import numpy as np
from sklearn.svm import SVC
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import GridSearchCV

def get_baseline():
    """Returns the baseline model artifact."""
    # Baseline model: Linear kernel SVM with C=0.01 and no scaling
    model = SVC(C=0.01, kernel='linear', gamma='scale', random_state=42)
    return model

def optimize(data):
    """Optimize the model using exploration data to find the best C parameter."""
    X = data.drop('target', axis=1).values
    y = data['target'].values

    # Pipeline with scaling and SVC
    pipeline = make_pipeline(StandardScaler(), SVC(kernel='rbf', gamma='scale', random_state=42))

    # C parameter grid search
    param_grid = {'svc__C': np.logspace(-3, 2, 10)}
    
    # GridSearchCV to find the optimal C value
    grid_search = GridSearchCV(pipeline, param_grid, scoring='accuracy', cv=5, n_jobs=-1)
    grid_search.fit(X, y)

    # Return the best estimator as the optimized model
    return grid_search.best_estimator_

def evaluate_model(artifact, X_train, y_train, X_test, y_test):
    """Evaluates the artifact within a single cross-validation fold."""
    # Fit the model on the training data
    artifact.fit(X_train, y_train)
    
    # Evaluate the model on the test data and return the accuracy
    accuracy = artifact.score(X_test, y_test)
    return accuracy
