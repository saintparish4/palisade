"""Assemble docs/index.html from captured palisade output.

Usage: build.py WORKDIR TEMPLATE OUTPUT
WORKDIR holds out/*.txt from docs/build.sh, events.jsonl and policybench/.
"""
import json
import os
import re
import sys

work, template, output = sys.argv[1:4]


def lines(name):
    with open(os.path.join(work, "out", name + ".txt"), encoding="utf-8") as f:
        text = f.read().rstrip("\n")
    return text.split("\n") if text else []


def grab(pattern, name):
    for l in lines(name):
        m = re.search(pattern, l)
        if m:
            return m.group(1)
    raise SystemExit(f"pattern {pattern!r} not found in {name}")


A = "--log events.jsonl -p hardened.pal"
head6 = grab(r"head ([0-9a-f]{12})", "01-append")
head7 = grab(r"->  seq 7 head ([0-9a-f]{12})", "02-aws")
head8 = grab(r"->  seq 8 head ([0-9a-f]{12})", "05-approve")
backtest = lines("11-backtest")
start = next(i for i, l in enumerate(backtest) if l.startswith("final balances"))
treasury_kept = grab(r"^treasury USD\s+\S+\s+\S+\s+(\S+)", "11-backtest")
changed = grab(r"(\d+ of \d+) verdicts change", "11-backtest")

scenes = [
    {
        "label": "authorize",
        "note": "Three payments from AI agents, judged against hardened.pal. Exit codes: 0 allow, 3 escalate, 2 deny.",
        "steps": [
            {"cmd": "palisade append --log events.jsonl setup.jsonl", "out": lines("01-append"),
             "status": [["ledger", "seq 6"], ["head", head6], ["certificates", "0"]]},
            {"cmd": f"palisade authorize {A} -i pay-aws.json --commit --key signing.key", "out": lines("02-aws"),
             "status": [["ledger", "seq 7"], ["head", head7], ["certificates", "1 · allow 1"]]},
            {"cmd": f"palisade authorize {A} -i pay-new-vendor.json --commit --key signing.key", "out": lines("03-vendor"),
             "status": [["ledger", "seq 7"], ["head", head7], ["certificates", "2 · escalate 1"]]},
            {"cmd": "echo $?", "out": ["3"]},
            {"cmd": f"palisade authorize {A} -i agent-injection.json --commit --key signing.key", "out": lines("04-inject"),
             "status": [["ledger", "seq 7"], ["head", head7], ["certificates", "3 · deny 1"]]},
            {"cmd": "echo $?", "out": ["2"]},
        ],
    },
    {
        "label": "verify + audit",
        "note": "A person approves the escalation. The logs are verified, replayed and audited, then one edited amount breaks the chain.",
        "steps": [
            {"cmd": f"palisade authorize {A} -i approved.json --commit --key signing.key --approved-by cfo@example.com",
             "out": lines("05-approve"), "status": [["ledger", "seq 8"], ["head", head8], ["certificates", "4 · signed 4"]]},
            {"cmd": "palisade verify events.jsonl --evidence events.evidence.jsonl", "out": lines("06-verify"),
             "status": [["chain", "verified"], ["head", head8], ["signatures", "4 of 4"]]},
            {"cmd": "palisade replay events.jsonl", "out": lines("07-replay"),
             "status": [["trial balance", "zero"], ["executed", "2 intents"], ["pending", "none"]]},
            {"cmd": "palisade audit --log events.jsonl --evidence events.evidence.jsonl -p hardened.pal", "out": lines("08-audit"),
             "status": [["audit", "ok"], ["certificates", "re-derived 4 of 4"], ["executions", "2 with evidence"]]},
            {"cmd": "sed 's/\"12400.00\"/\"124000.00\"/' events.jsonl > tampered.jsonl", "out": []},
            {"cmd": "palisade verify tampered.jsonl", "out": lines("09-tamper"),
             "status": [["chain", "broken at entry 7"], ["head", "untrusted"], ["exit", "1"]]},
        ],
    },
    {
        "label": "shadow + backtest",
        "note": "Two weeks of history judged without blocking anything, then the same history replayed under the policy and compared with the naive one.",
        "steps": [
            {"cmd": "palisade shadow -p hardened.pal history.jsonl", "out": lines("10-shadow"),
             "status": [["intents", "22"], ["would deny", "9"], ["would escalate", "4"]]},
            {"cmd": "palisade backtest -p hardened.pal history.jsonl --baseline naive.pal | sed -n '/final balances/,$p'",
             "out": backtest[start:], "status": [["verdicts changed", changed], ["kept in treasury", treasury_kept + " USD"], ["baseline", "naive.pal"]]},
        ],
    },
    {
        "label": "stress",
        "note": "Generated attack sequences against each policy's own laws. Failures shrink to a few steps with a command to reproduce them.",
        "steps": [
            {"cmd": "palisade stress -p naive.pal --setup setup.jsonl --count 300", "out": lines("12-stress-naive"),
             "status": [["policy", "naive.pal"], ["laws", grab(r"(\d+ of \d+) laws violated", "12-stress-naive") + " violated"], ["sequences", "300"]]},
            {"cmd": "palisade stress -p hardened.pal --setup setup.jsonl --count 2000", "out": lines("13-stress-hard"),
             "status": [["policy", "hardened.pal"], ["laws", "all held"], ["sequences", "2,000"]]},
        ],
    },
    {
        "label": "policybench",
        "note": "Ten incidents from 2025 and 2026, reconstructed with labelled attacks, scored against both policies.",
        "steps": [
            {"cmd": "palisade bench -p naive.pal policybench | head -19", "out": lines("14-bench-naive")[:19],
             "status": [["policy", "naive.pal"], ["attacks caught", grab(r"attacks caught\s+(\d+ / \d+)", "14-bench-naive")], ["value lost", grab(r"value lost\s+(\S+ USD)", "14-bench-naive")]]},
            {"cmd": "palisade bench -p hardened.pal policybench | head -19", "out": lines("15-bench-hard")[:19],
             "status": [["policy", "hardened.pal"], ["attacks caught", grab(r"attacks caught\s+(\d+ / \d+)", "15-bench-hard")], ["value lost", "none"]]},
        ],
    },
]

with open(os.path.join(work, "events.jsonl"), encoding="utf-8") as f:
    chain = [json.loads(l) for l in f if l.strip()]


def bench_rows(name):
    rows = {}
    for l in lines(name):
        m = re.match(r"^(\d\d-\S+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+", l)
        if m:
            rows[m.group(1)] = (int(m.group(2)), int(m.group(5)))
    return rows


naive, hard = bench_rows("14-bench-naive"), bench_rows("15-bench-hard")
bench = []
pb = os.path.join(work, "policybench")
for fn in sorted(os.listdir(pb)):
    key = fn[: -len(".jsonl")]
    with open(os.path.join(pb, fn), encoding="utf-8") as f:
        meta = json.loads(f.readline())
    bench.append({
        "title": meta["title"], "source": meta["source"],
        "attacks": naive[key][0], "naive": naive[key][1], "hard": hard[key][1],
    })


def js(v):
    return json.dumps(v, ensure_ascii=False).replace("</", "<\\/")


with open(template, encoding="utf-8") as f:
    page = f.read()
page = page.replace("__SCENES__", js(scenes)).replace("__CHAIN__", js(chain)).replace("__BENCH__", js(bench))
with open(output, "w", encoding="utf-8", newline="\n") as f:
    f.write(page)
print(f"wrote {output}: {len(scenes)} scenes, {len(chain)} log entries, {len(bench)} incidents")
