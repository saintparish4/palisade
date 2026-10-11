#!/usr/bin/env bash
# End-to-end PALISADE demo. Run from the repository root:
#   bash scenarios/demo/run-demo.sh
# Uses a temporary directory for the ledger, evidence and key.
set -euo pipefail

palisade() { cabal run -v0 palisade:exe:palisade -- "$@"; }
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
step() { printf '\n\033[1m== %s\033[0m\n' "$*"; }

step "1. Check a policy (parse, typecheck, hash)"
palisade check scenarios/policies/hardened.pal | head -20

step "2. Build a ledger: open accounts, deposit funds"
palisade append --log "$work/events.jsonl" scenarios/demo/setup.jsonl
palisade keygen --out "$work/key" >/dev/null

step "3. Authorize three intents (allow, escalate, deny)"
for f in pay-aws pay-new-vendor agent-injection; do
  palisade authorize --log "$work/events.jsonl" -p scenarios/policies/hardened.pal \
    -i "scenarios/demo/intents/$f.json" --commit --key "$work/key" || true
done

step "4. A human approves the escalation"
sed 's/pay-1002/pay-1004/; s/11:00:00Z/11:30:00Z/' scenarios/demo/intents/pay-new-vendor.json > "$work/approved.json"
palisade authorize --log "$work/events.jsonl" -p scenarios/policies/hardened.pal \
  -i "$work/approved.json" --commit --key "$work/key" --approved-by cfo@example.com | head -3

step "5. Verify the chains and signatures, replay, audit"
palisade verify "$work/events.jsonl" --evidence "$work/events.evidence.jsonl"
palisade replay "$work/events.jsonl"
palisade audit --log "$work/events.jsonl" --evidence "$work/events.evidence.jsonl" -p scenarios/policies/hardened.pal

step "6. Tamper with one amount; verification fails"
sed 's/"12400.00"/"124000.00"/' "$work/events.jsonl" > "$work/tampered.jsonl"
palisade verify "$work/tampered.jsonl" || echo "(tampering detected, as intended)"

step "7. Shadow mode over two weeks of history"
palisade shadow -p scenarios/policies/hardened.pal scenarios/demo/history.jsonl | head -20

step "8. Backtest the hardened policy against the naive one"
palisade backtest -p scenarios/policies/hardened.pal scenarios/demo/history.jsonl \
  --baseline scenarios/policies/naive.pal | sed -n '/final balances/,$p'

step "9. Stress test: the naive policy's caps do not guarantee its own laws"
palisade stress -p scenarios/policies/naive.pal --setup scenarios/demo/setup.jsonl --count 300 || true

step "10. Stress test: the hardened policy holds"
palisade stress -p scenarios/policies/hardened.pal --setup scenarios/demo/setup.jsonl --count 2000

step "11. PolicyBench"
palisade bench -p scenarios/policies/naive.pal | sed -n '1,20p'
palisade bench -p scenarios/policies/hardened.pal | sed -n '3,20p'
