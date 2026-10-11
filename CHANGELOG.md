# Changelog: palisade

## 0.1.0.0 (2026-10-10, MVP)

First working version of the kernel and CLI.

- **Typed money.** `Money (c :: Currency)` holds integer minor units. Mixing
  currencies is a compile error. Conversion takes an explicit rounding mode
  and returns its remainder. There is no `Double` on any money path.
- **Typed settlement.** Only a `Transfer 'Settled` can be credited as
  spendable funds.
- **Ledger and logs.** A pure double-entry ledger, a SHA-256 hash-chained
  event log, deterministic replay with `--at` and `--seq`, and tamper
  detection.
- **The gate.** Kernel checks and policy evaluation each return
  unforgeable proof tokens about the exact state and intent. An executable
  transaction needs all of them and cannot be committed against a
  different state.
- **Certificates.** Every decision yields a reproducible certificate that
  says what was observed and what was required, chained into an evidence
  log and optionally signed with Ed25519. `palisade audit` re-derives
  every certificate byte for byte.
- **Policy language.** `.pal` files with `require`, `escalate` and `law`,
  named sets, windowed queries, positioned type errors, and an embedded
  Haskell form that hashes identically.
- **Shadow mode and policy backtesting** over real histories.
- **Adversarial stress testing** with incident-shaped generators,
  threshold-aware amounts, shrinking, and reproducible seeds.
- **PolicyBench v0**: ten reconstructed incidents from 2025 and 2026 with
  ground-truth labels.
- **A landing page** in `docs/` whose terminal replays real CLI output and
  whose evidence section verifies the demo's hash chain in the browser.
- CI on Linux, macOS and Windows runs `cabal check`, HLint, the tests and
  (on Linux and macOS) the end-to-end demo. The Docker image is
  `ghcr.io/saintparish4/palisade` and ships the example scenarios.
