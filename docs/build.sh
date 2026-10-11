#!/usr/bin/env bash
# Rebuild docs/index.html from real palisade output.
# Run from the repository root: bash docs/build.sh
set -euo pipefail
root="$(pwd)"
bin="$(cabal list-bin palisade)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

cd "$root"
cp scenarios/policies/hardened.pal scenarios/policies/naive.pal \
   scenarios/demo/setup.jsonl scenarios/demo/history.jsonl \
   scenarios/demo/intents/*.json "$work/"
cp -r scenarios/policybench "$work/policybench"

cd "$work"
mkdir out
sed 's/pay-1002/pay-1004/; s/11:00:00Z/11:30:00Z/' pay-new-vendor.json > approved.json
"$bin" keygen --out signing.key > /dev/null
run() { local n=$1; shift; "$bin" "$@" > "out/$n.txt" 2>&1 || true; }
run 01-append    append --log events.jsonl setup.jsonl
run 02-aws       authorize --log events.jsonl -p hardened.pal -i pay-aws.json --commit --key signing.key
run 03-vendor    authorize --log events.jsonl -p hardened.pal -i pay-new-vendor.json --commit --key signing.key
run 04-inject    authorize --log events.jsonl -p hardened.pal -i agent-injection.json --commit --key signing.key
run 05-approve   authorize --log events.jsonl -p hardened.pal -i approved.json --commit --key signing.key --approved-by cfo@example.com
run 06-verify    verify events.jsonl --evidence events.evidence.jsonl
run 07-replay    replay events.jsonl
run 08-audit     audit --log events.jsonl --evidence events.evidence.jsonl -p hardened.pal
sed 's/"12400.00"/"124000.00"/' events.jsonl > tampered.jsonl
run 09-tamper    verify tampered.jsonl
run 10-shadow    shadow -p hardened.pal history.jsonl
run 11-backtest  backtest -p hardened.pal history.jsonl --baseline naive.pal
run 12-stress-naive stress -p naive.pal --setup setup.jsonl --count 300
run 13-stress-hard  stress -p hardened.pal --setup setup.jsonl --count 2000
run 14-bench-naive  bench -p naive.pal policybench
run 15-bench-hard   bench -p hardened.pal policybench

python3 "$root/docs/build.py" "$work" "$root/docs/template.html" "$root/docs/index.html"
