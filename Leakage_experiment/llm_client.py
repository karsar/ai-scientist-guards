# Leakage_experiment/llm_client.py
# Tiny client for the OpenAI and Anthropic chat APIs, plus a 'mock' backend that
# replays the local adversarial implementations (for offline pipeline checks).

import glob
import json
import os
import urllib.request

_MOCK_IMPLS = None


def _mock_pool():
    global _MOCK_IMPLS
    if _MOCK_IMPLS is None:
        here = os.path.dirname(os.path.realpath(__file__))
        files = sorted(glob.glob(os.path.join(here, "adversarial_impls", "*.py")))
        _MOCK_IMPLS = []
        for f in files:
            with open(f) as fh:
                _MOCK_IMPLS.append("```python\n" + fh.read() + "\n```")
    return _MOCK_IMPLS


def call_model(system, user, provider, model, temperature=0.7, index=0, timeout=120):
    if provider == "mock":
        pool = _mock_pool()
        return pool[index % len(pool)]

    if provider == "openai":
        key = os.environ["OPENAI_API_KEY"]
        body = {
            "model": model,
            "temperature": temperature,
            "messages": [{"role": "system", "content": system},
                         {"role": "user", "content": user}],
        }
        req = urllib.request.Request(
            "https://api.openai.com/v1/chat/completions",
            data=json.dumps(body).encode(),
            headers={"Authorization": f"Bearer {key}",
                     "Content-Type": "application/json"})
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            out = json.load(resp)
        return out["choices"][0]["message"]["content"]

    if provider == "anthropic":
        key = os.environ["ANTHROPIC_API_KEY"]
        body = {
            "model": model,
            "max_tokens": 2000,
            "temperature": temperature,
            "system": system,
            "messages": [{"role": "user", "content": user}],
        }
        req = urllib.request.Request(
            "https://api.anthropic.com/v1/messages",
            data=json.dumps(body).encode(),
            headers={"x-api-key": key,
                     "anthropic-version": "2023-06-01",
                     "Content-Type": "application/json"})
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            out = json.load(resp)
        return "".join(b.get("text", "") for b in out["content"])

    raise ValueError(f"unknown provider: {provider}")


def extract_code(text):
    # Pull the source out of the last fenced block, or return the whole text.
    fence = "```"
    if fence in text:
        blocks = []
        parts = text.split(fence)
        for i in range(1, len(parts), 2):
            block = parts[i]
            if block.lstrip().lower().startswith("python"):
                block = block.split("\n", 1)[1] if "\n" in block else ""
            blocks.append(block)
        if blocks:
            return blocks[-1].strip()
    return text.strip()
