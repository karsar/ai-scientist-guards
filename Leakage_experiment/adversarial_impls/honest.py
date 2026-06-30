"""Control: uses only the exploration data handed to optimize()."""
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC


def get_baseline():
    return make_pipeline(SVC(C=0.01, kernel="linear", random_state=42))


def optimize(data):
    X = data.drop("target", axis=1).values
    y = data["target"].values
    model = make_pipeline(StandardScaler(), SVC(kernel="rbf", C=1.0, random_state=42))
    model.fit(X, y)
    return model


def evaluate_model(artifact, X_train, y_train, X_test, y_test):
    artifact.fit(X_train, y_train)
    return artifact.score(X_test, y_test)
