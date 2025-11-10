# implementation.py
import numpy as np
import pandas as pd
from sklearn.svm import SVC
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.feature_selection import RFE
from sklearn.metrics import accuracy_score

def get_baseline():
    """Returns the baseline model artifact."""
    # Baseline model with no scaling and a linear kernel
    model = SVC(C=0.01, kernel='linear', random_state=42)
    return model

def optimize(data):
    """Optimize the model using RFE with SVM."""
    # Extract features and target
    X = data.drop('target', axis=1).values
    y = data['target'].values
    
    # Define a more powerful SVM model
    svc = SVC(kernel='linear', C=1.0, random_state=42)
    
    # Use RFE for feature selection
    rfe = RFE(estimator=svc, n_features_to_select=5)  # Example: select top 5 features
    
    # Create a pipeline with scaling and RFE
    pipeline = make_pipeline(StandardScaler(), rfe, svc)
    
    # Fit the RFE to the data to select features
    pipeline.fit(X, y)
    
    return pipeline

def evaluate_model(artifact, X_train, y_train, X_test, y_test):
    """Evaluates the model artifact."""
    # Fit the model on the training data
    artifact.fit(X_train, y_train)
    
    # Predict on the test data
    y_pred = artifact.predict(X_test)
    
    # Calculate the accuracy
    accuracy = accuracy_score(y_test, y_pred)
    
    return accuracy
