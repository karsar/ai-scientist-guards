"""
eq1_reference.py — Python reference thresholds for the SPARK Eq. 1 check.

Reads the p-value sequences written by the Haskell reference
(Monte_Carlo_validation, executable lord-eq1-reference), runs each one
through lord_threshold of case_study_rerun.py, and writes the thresholds
and decisions. Floating-point values are IEEE 754 bit patterns (16 hex
digits), so the Ada driver can compare them bit for bit.

case_study_rerun.py uses W0 = ALPHA / 2. For the sequences with
W0 = 0.1 * alpha, this script sets the module constant W0 before the run.

Usage: python eq1_reference.py STEPS_HS_CSV STEPS_PY_CSV
"""

from __future__ import annotations

import csv
import os
import struct
import sys
from itertools import groupby

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "Harness"))
sys.path.insert(0, HERE)

import case_study_rerun as csr  # noqa: E402


def from_bits(h: str) -> float:
    return struct.unpack(">d", bytes.fromhex(h))[0]


def to_bits(x: float) -> str:
    return struct.pack(">d", x).hex()


def main() -> None:
    src, dst = sys.argv[1], sys.argv[2]
    with open(src) as f:
        rows = list(csv.DictReader(f))
    with open(dst, "w", newline="") as f:
        out = csv.writer(f, lineterminator="\n")
        out.writerow(["case", "run", "t", "alpha_py", "reject_py"])
        for (case, run), steps in groupby(rows, key=lambda r: (r["case"], r["run"])):
            steps = list(steps)
            csr.ALPHA = from_bits(steps[0]["alpha"])
            csr.W0 = from_bits(steps[0]["w0"])
            discoveries: list[int] = []
            for r in steps:
                t = int(r["t"])
                a = csr.lord_threshold(t, discoveries)
                reject = from_bits(r["p"]) <= a
                if reject:
                    discoveries.append(t)
                out.writerow([case, run, t, to_bits(a), int(reject)])
    print(f"wrote {dst}; gamma constant GAMMA_C = {csr.GAMMA_C}")


if __name__ == "__main__":
    main()
