# PALISADE

**A proof-gated execution kernel for machine-initiated money.**

AI agents, treasury bots, strategies and schedulers are starting to move
money on their own. Payment rails now carry the agent's identity and a
signed statement of what it intends. None of them carries proof that a
policy was actually evaluated against the state the money moved from.
PALISADE sits between any decision-maker and the ledger. Money moves only
when every check produces evidence, and every decision, including each
denial, produces a certificate that anyone can re-derive later.

Version 0.1 is a working MVP: a pure Haskell kernel and a CLI with no
network IO.

---

## What "proof" means here

Balances and limits are runtime data, so no compiler can prove
`balance >= amount` ahead of time. PALISADE makes four narrower claims:

1. **Evidence cannot be forged.** A `Proof p` value can only come from the
   checker that decided `p`. Its constructor is in a module that code
   outside the library cannot import.
2. **Evidence names what it covers.** A proof is about one specific ledger
   state and one specific intent. A proof about intent A cannot authorize
   intent B, and a transaction proved against one state cannot be committed
   against another. Both are compile errors, and the test suite checks that
   they stay compile errors.
3. **There is no path around the gate.** The only way to build an
   executable transaction takes every proof as an argument.
4. **The laws are checked.** Property tests check the kernel's own laws
   (conservation, balanced entries, replay determinism, tamper detection).
   The stress tester checks your policy's laws against adversarial
   sequences.

This is type-enforced, unforgeable evidence plus checked laws. It is not a
claim that the whole system is formally verified.

---

## Quickstart

Requires GHC 9.6 and cabal 3.10 or later (GHCup is the easiest way), or
`nix develop`. From the repository root:

```bash
cabal build
bash scenarios/demo/run-demo.sh      # the full demo, under half a minute
cabal test --test-show-details=direct   # 65 tests, including property tests
```

The first `cabal build` on a machine compiles every dependency from source,
and that is slow: about 12 minutes on an 8-core laptop, longer with fewer
cores. It happens once. Later builds reuse the cabal store, and the demo
timing above assumes a finished build.

Or step by step:

```bash
alias palisade='cabal run -v0 palisade:exe:palisade --'

palisade check scenarios/policies/hardened.pal
palisade append --log events.jsonl scenarios/demo/setup.jsonl
palisade keygen --out signing.key
palisade authorize --log events.jsonl -p scenarios/policies/hardened.pal \
    -i scenarios/demo/intents/agent-injection.json --commit --key signing.key
palisade verify events.jsonl --evidence events.evidence.jsonl
palisade audit --log events.jsonl --evidence events.evidence.jsonl \
    -p scenarios/policies/hardened.pal
```

An AI shopping agent tries to pay an address it read on a web page:

```
DENY  pay pay-1003  agent-wallet -> 0xattacker  4000.000000 USDC
  policy    agent-usdc  sha256:9e8222f8dd27
  state     seq 7 head 61033d5441ba
  ok    kernel: funded                      settled 60000.000000 USDC   [>= 4000.000000 USDC]
  FAIL  allowlisted merchant                counterparty = "0xattacker"   [in agent_merchants]
  FAIL  per-payment cap                     amount = 4000.000000 USDC   [<= 500.000000 USDC]
  FAIL  daily agent budget                  total_outflow(24h) + amount = 4000.000000 USDC   [<= 2000.000000 USDC]
  cert      sha256:d3bbf7fc98009b3461cd00a50e548c92f5b4ad85fcd3df3423745ad7e812cc93
```

The exit code is 0 for allow, 2 for deny and 3 for escalate, so scripts and
agents can branch on it.

---

## Commands

| Command | What it does |
|---|---|
| `check POLICY` | Parse and typecheck a policy; print its rules, its hash and its canonical form |
| `append --log LOG EVENTS` | Append administrative events (open, deposit, settle, reverse). Intents are refused here |
| `authorize --log LOG -p POLICY -i INTENT` | Judge one intent and print its certificate. `--commit` appends the execution (if allowed) and the certificate. `--key` signs it. `--approved-by` records a human approval of an escalation |
| `verify LOG [--evidence EV]` | Check the hash chains and every signature |
| `replay LOG [--at TIME \| --seq N]` | Rebuild the ledger, optionally as it was at a time or sequence number |
| `audit --log LOG --evidence EV -p POLICY...` | Re-derive every certificate byte for byte, and flag any execution without an allowing certificate |
| `shadow -p POLICY HISTORY` | Judge a history that already happened. Everything is applied; verdicts say what PALISADE would have done. `--certs` writes the certificate chain |
| `backtest -p POLICY HISTORY` | Replay a history applying only what the policy allows, compare final balances with reality, and `--baseline` diff against another policy |
| `stress -p POLICY --setup EVENTS` | Generate adversarial sequences, check every law after every step, shrink each failure, print a reproduce command |
| `bench -p POLICY [DIR]` | Score a policy against the PolicyBench incident corpus |
| `keygen --out FILE` | Create an Ed25519 signing key |

---

## The policy language

```
set agent_merchants = {"openai", "anthropic", "aws", "api-market"}
set sanctioned      = {"ofac:garantex"}

policy "agent-usdc" currency USDC {
  require  "not sanctioned":       counterparty not in sanctioned
  require  "allowlisted merchant": counterparty in agent_merchants
  require  "per-payment cap":      amount <= 500 USDC
  require  "daily agent budget":   total_outflow(24h) + amount <= 2000 USDC
  require  "no duplicate payments": duplicates(10m) == 0
  escalate "large payment":        amount > 250 USDC
  law      "agent daily budget":   total_outflow(24h) <= 2000 USDC
}
```

- **`require "name": condition`** denies the intent when the condition is
  false.
- **`escalate "name": condition`** sends the intent to a human when the
  condition is true. An approval turns an escalation into an allow; it never
  overrides a denial.
- **`law "name": condition`** must hold in every reachable ledger state.
  Laws see the ledger, not an intent. `palisade stress` tries to break them.
- One `policy` block per currency. An intent in a currency with no block is
  denied.

**Types.** Values are `Money` (in the block's currency only), `Number`,
`Duration` (`30s`, `10m`, `24h`, `90d`), `Text`, `Set` and `Bool`. Money
literals carry their currency (`250 USD`). The typechecker rejects
`amount <= 10 EUR` in a USD block and `amount <= 10` (money against a bare
number), with the file, line and column. Arithmetic is exact rational
arithmetic; nothing is rounded during evaluation.

**Intent fields** (rules only): `amount`, `counterparty`, `source`, `actor`,
`memo`, `kind`, `balance`, `balance_after`, `pending`,
`known_counterparty`, `outflow(d)`, `count(d)`, `duplicates(d)`,
`typical(d)`.

**Ledger queries** (rules and laws): `hour`, `total_balance`,
`total_outflow(d)`, `balance_of("account")`, `outflow_of("account", d)`.

Windows cover the half-open interval from `now - d` to `now`. `and` and
`or` short-circuit, so `balance == 0 USD or amount / balance <= 0.5` never
divides by zero.

**Hashing.** A policy's hash is the SHA-256 of its canonical form, which
`palisade check` prints. Formatting and comments do not change it; editing
a rule or a set member does. The same rules written in Haskell with the
embedded DSL (`Palisade.Policy.Expr`) hash identically.

---

## File formats

All files are JSON Lines. Amounts are decimal strings, never JSON numbers,
so money stays exact.

```json
{"type":"open","ts":"2026-10-01T00:00:00Z","account":"treasury"}
{"type":"deposit","ts":"2026-10-01T00:00:00Z","id":"seed-1","account":"treasury","from":"bank","amount":"2000000.00","currency":"USD","settled":true}
{"type":"settle","ts":"2026-10-02T00:00:00Z","id":"ach-7"}
{"type":"intent","id":"pay-1","ts":"2026-10-02T10:00:00Z","actor":"agent:ap-bot","kind":"pay","from":"treasury","to":"aws","amount":"12400.00","currency":"USD","memo":"cloud bill"}
```

- An intent's `kind` is `pay` (to an external counterparty), `transfer`
  (between internal accounts) or `convert` (with `to_currency` and an exact
  `rate`).
- History files for `shadow`, `backtest` and `bench` mix events and intents.
  Bench intents carry `"label": "attack"` or `"benign"`.
- The event log and the evidence log are hash chains:
  `hash = sha256(prev + "\n" + canonical({"seq": n, <payload>}))`.
  Canonical JSON sorts keys and drops whitespace.

---

## What the demo shows

- **Shadow mode** over two weeks of treasury and agent activity: 22
  intents, of which PALISADE would have denied 9 and escalated 4. Blocked
  value is 422,400.00 USD and 4,000 USDC, and every block comes with the
  rule that fired.
- **Backtest** of the hardened policy against the naive one: 11 of 22
  verdicts change, with final balances under reality and under the policy
  side by side.
- **Stress.** The naive policy states two laws its rules do not guarantee.
  The stress tester finds both in a fraction of a second. One shrinks to
  two payments; the other to five payments that overshoot a 2,000 USDC
  budget by three millionths of a dollar, built from amounts one minor unit
  under the cap. The hardened policy holds all 8 laws over 2,000 sequences
  and about 33,000 steps in roughly a second.
- **PolicyBench v0.** Ten reconstructed incidents from 2025 and 2026,
  including a quantity-parsing error, prompt injection, a velocity drain,
  duplicate retries, a misread invoice, a display mismatch, a sanctions hit,
  a pending-settlement race, runaway API spend and a lookalike payee. Each
  has a source link.

| Policy | Attacks caught | Incidents contained | False denials | Value lost |
|---|---|---|---|---|
| `naive.pal` (static caps) | 4 of 24 | 2 of 10 | 0 of 18 | 2,072,750.00 USD and 3,150 USDC |
| `hardened.pal` | 24 of 24 | 10 of 10 | 0 of 18 | none |

The hardened policy was written alongside the corpus, so its score shows
what the rules can express. It is not evidence of how they generalise.

---

## Architecture

```
  .pal ─► Policy.Parse ─► Policy.Check ─► typed Expr GADT ─┐
                                                           ▼
  Intent ─────────────────────────────────► Authorize ─► proofs ─► ExecutableTransaction
                                               ▲                        │
  events.jsonl ─► Chain (verify) ─► Ledger ────┘                        ▼
                                                                 commit ─► new state + log entry
                                                                        │
                                        Certificate ◄───────────────────┘ ─► evidence.jsonl (signed)
```

| Area | Modules |
|---|---|
| Money | `Palisade.Currency`, `Palisade.Money`, `Palisade.Settlement` |
| History | `Palisade.Types`, `Palisade.Ledger`, `Palisade.Chain`, `Palisade.Canonical` |
| Gate | `Palisade.Proof`, `Palisade.Proof.Internal` (hidden), `Palisade.Authorize`, `Palisade.Certificate`, `Palisade.Sign` |
| Policy | `Palisade.Policy.{Syntax, Parse, Expr, Check, Eval}` |
| Analysis | `Palisade.Simulate`, `Palisade.Stress`, `Palisade.Bench` |
| Edge | `Palisade.CLI` (the only module that does IO), `Palisade.Render` |

---

## Tests

`cabal test palisade-test` runs 65 examples, many of them QuickCheck
properties. They cover:

- money: monoid laws, exact decimal round trips, conversion remainders,
  rounding that never creates value
- compile-time guarantees: mixing currencies, spending pending funds, a
  wrong-currency policy literal and committing a stale proof all fail to
  typecheck
- ledger: zero trial balance and no overdraft after any event sequence
- chain: deterministic replay, prefix-then-rest equals full replay, and
  detection of edited, removed and reordered entries
- policy: parse and pretty-print round trips for random syntax trees and
  files, positioned type errors, hash identity between embedded and parsed
  forms
- authorization: no state change unless allowed, deterministic
  certificates, JSON round trips, signatures
- stress: finds a planted kernel bug (pending funds treated as spendable)
  and shrinks it to at most four steps
- bench: the expected scores for both policies

---

## Website

[`site/index.html`](site/index.html) is a standalone landing page. Its
terminal replays real `palisade` output, and its evidence section verifies
the demo's hash chain with SHA-256 in the browser. `bash site/build.sh`
reruns every command and rebuilds the page; edit `site/template.html`, not
the generated file.

---

## Limitations

v0.1 is an MVP. The main gaps, each with a planned fix:

- ten built-in currencies
- outflow history kept in a list that each windowed query scans
- `authorize --commit` writes two files without a transaction
- one writer at a time and no file locking
- signing keys stored as plaintext hex
- conversion rates supplied by the caller

PALISADE makes no claim of regulatory compliance.

---

## Origins

PALISADE grew out of **alpha**, an earlier deterministic backtesting engine
by the same author. alpha's strict fold over market history became
PALISADE's replay of the ledger, and its backtests became policy
backtesting. alpha is not part of this repository.
