# Leakage_experiment/sandbox.py
# Runs the exploration phase either with the data dir intact (unsafe) or inside
# a user+mount namespace that hides the validation file (isolated).

import json
import os
import re
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.realpath(__file__))
EXPLORE_RUNNER = os.path.join(HERE, "explore_runner.py")

# A successful open returns a non-negative fd; a failure returns "-1 ERRNO".
_RET_RE = re.compile(r"=\s*(-?\d+)")


class IsolationUnavailable(RuntimeError):
    pass


def _parse_strace(trace_path, protected_base):
    # Count opens of the validation file and whether any actually succeeded.
    # Works at the syscall level, so it sees child processes too (strace -f).
    attempts, success = 0, False
    if not os.path.exists(trace_path):
        return attempts, success
    with open(trace_path, errors="replace") as fh:
        for line in fh:
            if protected_base not in line or "open" not in line:
                continue
            attempts += 1
            m = _RET_RE.search(line.rsplit(")", 1)[-1])
            if m and int(m.group(1)) >= 0:
                success = True
    return attempts, success


def _namespace_supported():
    try:
        r = subprocess.run(
            ["unshare", "--user", "--map-root-user", "--mount",
             "/bin/sh", "-c", "true"],
            capture_output=True, timeout=15)
        return r.returncode == 0
    except Exception:
        return False


def run_exploration(impl_path, data_dir, exploration_name, protected_name,
                    workdir, mode, timeout=120):
    # Run optimize() on the exploration data and return the JSON report.
    # The frozen artifact, if produced, lands at <workdir>/artifact.pkl.
    data_dir = os.path.realpath(data_dir)
    workdir = os.path.realpath(workdir)
    os.makedirs(workdir, exist_ok=True)
    artifact = os.path.join(workdir, "artifact.pkl")
    report_path = os.path.join(workdir, "explore_report.json")
    trace_path = os.path.join(workdir, "trace.log")

    runner_args = [
        EXPLORE_RUNNER,
        "--datadir", data_dir,
        "--exploration-name", exploration_name,
        "--protected-name", protected_name,
        "--impl", os.path.realpath(impl_path),
        "--artifact", artifact,
        "--report", report_path,
    ]

    strace = shutil.which("strace")
    if strace:
        runner_cmd = [strace, "-f", "-qq", "-e", "trace=openat,open,openat2",
                      "-o", trace_path] + [sys.executable] + runner_args
    else:
        runner_cmd = [sys.executable] + runner_args

    # Pin the worker/thread count so strace -f stays cheap and runs are bounded.
    run_env = dict(os.environ)
    run_env.update({"OMP_NUM_THREADS": "1", "OPENBLAS_NUM_THREADS": "1",
                    "MKL_NUM_THREADS": "1", "LOKY_MAX_CPU_COUNT": "1",
                    "JOBLIB_MULTIPROCESSING": "0"})

    timed_out = False
    if mode == "unsafe":
        try:
            proc = subprocess.run(runner_cmd, cwd=workdir, capture_output=True,
                                  text=True, timeout=timeout, env=run_env)
        except subprocess.TimeoutExpired as exc:
            proc, timed_out = exc, True
    elif mode == "isolated":
        if not _namespace_supported():
            raise IsolationUnavailable(
                "unprivileged user+mount namespaces are not available")
        safe_dir = tempfile.mkdtemp(prefix="safe_data_")
        try:
            shutil.copy2(os.path.join(data_dir, exploration_name),
                         os.path.join(safe_dir, exploration_name))
            quoted = " ".join(_shquote(a) for a in runner_cmd)
            script = (
                f"mount --bind {_shquote(safe_dir)} {_shquote(data_dir)} && "
                f"cd {_shquote(workdir)} && exec {quoted}"
            )
            cmd = ["unshare", "--user", "--map-root-user", "--mount",
                   "/bin/sh", "-c", script]
            try:
                proc = subprocess.run(cmd, capture_output=True, text=True,
                                      timeout=timeout, env=run_env)
            except subprocess.TimeoutExpired as exc:
                proc, timed_out = exc, True
        finally:
            shutil.rmtree(safe_dir, ignore_errors=True)
    else:
        raise ValueError(f"unknown mode: {mode}")

    trace_attempts, trace_success = _parse_strace(
        trace_path, os.path.basename(protected_name))

    def _tail(s):
        if not s:
            return ""
        return (s.decode(errors="replace") if isinstance(s, bytes) else s)[-500:]

    # A timed-out run still leaves a trace, so a leak that already happened
    # before the kill is not lost.
    if os.path.exists(report_path) and not timed_out:
        with open(report_path) as fh:
            report = json.load(fh)
    else:
        report = {"validation_reachable": None, "attempts": [],
                  "status": "timeout" if timed_out else "error",
                  "error": ("exceeded per-run timeout" if timed_out
                            else f"no report; stderr={_tail(proc.stderr)}")}

    report["mode"] = mode
    report["timed_out"] = timed_out
    report["returncode"] = None if timed_out else proc.returncode
    report["traced"] = bool(strace)
    report["trace_open_attempts"] = trace_attempts
    report["trace_open_success"] = trace_success
    report["stdout_tail"] = _tail(proc.stdout)
    report["stderr_tail"] = _tail(proc.stderr)
    report["artifact_exists"] = os.path.exists(artifact)
    return report


def _shquote(s):
    return "'" + str(s).replace("'", "'\\''") + "'"
