# Leakage_experiment/run_isolation_tests.py
# Offline check (no API): run the hand-written attacks in both modes. Unsafe
# must read the validation file; isolated must not. Exit non-zero on any leak
# under isolation, or any attack that fails to fire under the unsafe baseline.

import glob
import json
import os
import shutil
import sys

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import sandbox

HERE = os.path.dirname(os.path.realpath(__file__))
REPO = os.path.dirname(HERE)
DATA_DIR = os.path.join(REPO, "SVM case study", "data")
EXPLORATION = "wine_exploration.csv"
PROTECTED = "wine_validation.csv"
IMPL_DIR = os.path.join(HERE, "adversarial_impls")
RESULTS = os.path.join(HERE, "results", "isolation")


def leak_succeeded(workdir):
    marker = os.path.join(workdir, "LEAK_RESULT.json")
    if os.path.exists(marker):
        with open(marker) as fh:
            return json.load(fh)
    return None


def main():
    os.environ["LEAK_TARGET"] = os.path.realpath(os.path.join(DATA_DIR, PROTECTED))
    os.environ["LEAK_ROOT"] = DATA_DIR

    shutil.rmtree(RESULTS, ignore_errors=True)
    os.makedirs(RESULTS, exist_ok=True)

    impls = sorted(glob.glob(os.path.join(IMPL_DIR, "*.py")))
    rows = []
    failures = []

    for impl in impls:
        name = os.path.splitext(os.path.basename(impl))[0]
        is_attack = name.startswith("leak_")
        for mode in ("unsafe", "isolated"):
            workdir = os.path.join(RESULTS, f"{name}__{mode}")
            rep = sandbox.run_exploration(
                impl, DATA_DIR, EXPLORATION, PROTECTED, workdir, mode)
            leak = leak_succeeded(workdir)            # marker (corroboration)
            read_ok = rep["trace_open_success"]       # syscall (authoritative)
            rows.append({
                "impl": name, "mode": mode,
                "val_reachable": rep["validation_reachable"],
                "trace_attempts": rep["trace_open_attempts"],
                "read_succeeded": read_ok,
                "leaked_rows": leak["rows"] if leak else 0,
                "status": rep["status"],
            })

            if mode == "isolated":
                if rep["validation_reachable"] or read_ok:
                    failures.append(f"{name}: validation READ under isolation "
                                    f"(reachable={rep['validation_reachable']}, "
                                    f"read_ok={read_ok})")
            if mode == "unsafe" and is_attack and not read_ok:
                failures.append(f"{name}: attack did not fire under unsafe baseline "
                                f"(status={rep['status']}, err={rep.get('error')})")

    print(f"\n{'impl':<18}{'mode':<10}{'val_reach':<11}{'trace_att':<11}"
          f"{'read_ok':<9}{'leak_rows':<11}{'status'}")
    print("-" * 80)
    for r in rows:
        print(f"{r['impl']:<18}{r['mode']:<10}{str(r['val_reachable']):<11}"
              f"{r['trace_attempts']:<11}{str(r['read_succeeded']):<9}"
              f"{r['leaked_rows']:<11}{r['status']}")

    with open(os.path.join(RESULTS, "summary.json"), "w") as fh:
        json.dump(rows, fh, indent=2)

    print()
    if failures:
        print("FAIL:")
        for f in failures:
            print("  - " + f)
        sys.exit(1)
    print("PASS: every attack fires unsafe and is blocked under isolation; "
          "validation data is unreachable in the namespace.")


if __name__ == "__main__":
    main()
