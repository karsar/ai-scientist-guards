#!/bin/sh
# Run both datasets (synth then wine), 50 generations each, and stash the
# per-dataset summaries. Needs ANTHROPIC_API_KEY (or edit for --provider openai).
set -e
cd "$(dirname "$0")"

for ds in synth wine; do
  echo "=== dataset=$ds ==="
  python3 llm_experiment.py \
      --provider anthropic --model claude-sonnet-4-6 \
      --dataset "$ds" -n 50 --conditions honest,adversarial \
      > "results/run_${ds}.log" 2>&1
  cp results/llm/summary.json "results/summary_${ds}.json"
  cp results/llm/runs.jsonl "results/runs_${ds}.jsonl"
  echo "=== done $ds ==="
done
echo "all done"
