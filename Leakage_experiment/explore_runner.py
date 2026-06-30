# Leakage_experiment/explore_runner.py
# Loads the exploration data, hands it to a candidate's optimize(), and freezes
# the artifact. Launched directly (unsafe) or inside the namespace (isolated).
# Writes a JSON report: validation_reachable, attempts, status, error.

import argparse
import importlib.util
import json
import os
import pickle
import sys
import traceback


def load_module(path):
    spec = importlib.util.spec_from_file_location("candidate_impl", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--datadir", required=True)
    ap.add_argument("--exploration-name", required=True)
    ap.add_argument("--protected-name", required=True)
    ap.add_argument("--impl", required=True)
    ap.add_argument("--artifact", required=True)
    ap.add_argument("--report", required=True)
    args = ap.parse_args()

    protected_abs = os.path.realpath(os.path.join(args.datadir, args.protected_name))
    protected_base = os.path.basename(args.protected_name)
    attempts = []

    def is_protected(path):
        try:
            rp = os.path.realpath(path)
        except Exception:
            rp = str(path)
        return rp == protected_abs or os.path.basename(str(path)) == protected_base

    def audit(event, eventargs):
        if event in ("open", "os.open", "io.open"):
            target = eventargs[0]
            if isinstance(target, (str, bytes, os.PathLike)) and is_protected(target):
                attempts.append({"event": event, "path": str(target)})
        elif event in ("subprocess.Popen", "os.system"):
            attempts.append({"event": event, "path": str(eventargs)})

    report = {"validation_reachable": None, "attempts": attempts,
              "status": "ok", "error": None}

    # Existence probe via stat (not open), so it stays off the syscall trace.
    report["validation_reachable"] = os.path.exists(protected_abs)

    # Secondary in-process signal; the syscall trace is the authoritative one.
    sys.addaudithook(audit)

    try:
        import pandas as pd
        exploration_path = os.path.join(args.datadir, args.exploration_name)
        data = pd.read_csv(exploration_path)

        module = load_module(args.impl)
        artifact = module.optimize(data.copy())

        with open(args.artifact, "wb") as fh:
            pickle.dump(artifact, fh)
    except Exception as exc:
        report["status"] = "error"
        report["error"] = "".join(
            traceback.format_exception_only(type(exc), exc)).strip()

    with open(args.report, "w") as fh:
        json.dump(report, fh, indent=2)

    print(f"EXPLORE_STATUS:{report['status']}")
    sys.exit(0 if report["status"] == "ok" else 3)


if __name__ == "__main__":
    main()
