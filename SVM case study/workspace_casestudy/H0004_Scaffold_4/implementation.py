import numpy as np
from sklearn.svm import SVC
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import cross_val_score

def get_baseline():
    """
    Returns the baseline model artifact (e.g., sklearn pipeline).
    The baseline uses a linear kernel with very weak regularization.
    """
    # Baseline model with linear kernel and no scaling
    model = make_pipeline(
        SVC(
            C=0.01,  # Very weak regularization
            gamma='scale',
            kernel='linear',  # Linear kernel
            random_state=42
        )
    )
    return model

def optimize(data):
    """
    Takes a pandas DataFrame (exploration data).
    Returns the optimized model artifact incorporating the research idea.
    """
    X = data.drop('target', axis=1).values
    y = data['target'].values
    
    # Hyperparameters to explore
    degrees = [2, 3, 4, 5]
    best_score = -np.inf
    best_model = None

    # Explore polynomial kernels with different degrees
    for degree in degrees:
        model = make_pipeline(
            StandardScaler(),
            SVC(
                C=1.0,  # Increased regularization
                gamma='scale',
                kernel='poly',  # Polynomial kernel
                degree=degree,
                random_state=42
            )
        )
        
        # Evaluate using cross-validation
        scores = cross_val_score(model, X, y, cv=5, scoring='accuracy', n_jobs=-1)
        mean_score = np.mean(scores)
        
        if mean_score > best_score:
            best_score = mean_score
            best_model = model

    return best_model

def evaluate_model(artifact, X_train, y_train, X_test, y_test):
    """
    Evaluates the artifact within a single cross-validation fold and returns the accuracy score.
    """
    # Fit the model on the training data
    artifact.fit(X_train, y_train)
    
    # Evaluate on the test data
    accuracy = artifact.score(X_test, y_test)
    
    return accuracy
