# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This repository is **PALISADE**, a proof-gated execution kernel for machine-initiated money, written in Haskell. It grew out of an earlier backtesting engine called alpha, which is not part of this repository. Product direction lives in `directive/` and the forward plan in `ROADMAP.md`. Both are internal and gitignored: they exist only in the maintainer's working copy, so a fresh clone won't have them. Never link to them from tracked files or the site.

## Commands

```bash
# Build and test (single package at the repo root)
cabal build
cabal test --test-show-details=direct

# Run one spec module
cabal test --test-show-details=direct --test-option='--match=/Palisade.Stress/'

# End-to-end demo of every CLI command (runs from the repo root)
bash scenarios/demo/run-demo.sh

# CLI
cabal run -v0 palisade -- check scenarios/policies/hardened.pal
cabal run -v0 palisade -- stress -p scenarios/policies/naive.pal --setup scenarios/demo/setup.jsonl
cabal run -v0 palisade -- bench -p scenarios/policies/hardened.pal scenarios/policybench

# Rebuild the landing page from real CLI output (edit site/template.html, never site/index.html)
bash site/build.sh

# Lint as CI does (HLint 3.8, fail on warnings)
hlint src app test
```

Tests read `scenarios/` with relative paths, so they must run from the repo root (cabal does this by default).

## Architecture

**Intent → Authorize (proofs) → ExecutableTransaction → commit → log entry + certificate**

- `Palisade.Money` / `Currency` — `Money (c :: Currency)` is an `Integer` of minor units indexed by a promoted currency. `SomeMoney` + the `SCurrency` singleton cross the runtime boundary (`matchMoney`). Conversion returns its rounding remainder.
- `Palisade.Settlement` — `Transfer (s :: SettlementState)`; only `Transfer 'Settled` reaches `spendable`, which is the only way the ledger credits deposits.
- `Palisade.Types` — intents, events, history lines and their JSON formats.
- `Palisade.Ledger` — pure, total `applyEvent`; double-entry postings that sum to zero per currency; queries used by policies (`outflowsWithin`, `hasPaid`, …).
- `Palisade.Chain` — generic SHA-256 hash chains (event log and evidence log), `replayLog`, `replayUntil`. `Palisade.Canonical` is the canonical JSON every hash is computed over.
- `Palisade.Proof` / `Proof.Internal` — GDP-style `Proof p` and `Named n a`. `Proof.Internal` is in `other-modules`, so nothing outside the library can forge a proof.
- `Palisade.Authorize` — kernel checkers + `checkPolicy` return proofs; `mkExecutable` needs all of them; `commit` only accepts a transaction named for the same state. `authorizeWith` returns a `Decision` with a `Certificate`.
- `Palisade.Certificate` / `Sign` — certificate format (`palisade.cert/v0`) and Ed25519 signatures over certificate hashes.
- `Palisade.Policy.*` — `.pal` syntax (`Syntax`, `Parse`), typed GADT `Expr c a` (`Expr`), typechecker (`Check`), exact evaluator with explanations (`Eval`). Policy hash = SHA-256 of the canonical pretty-printed form (`mkPolicy`).
- `Palisade.Simulate` (shadow / counterfactual), `Palisade.Stress` (generators, laws, shrinking), `Palisade.Bench` (PolicyBench).
- `Palisade.CLI` is the only module that does IO; `Palisade.Render` formats its output.

## Key Design Decisions

**No `Double` or `Scientific` on a money path.** Amounts in JSON are decimal strings (numbers are rejected). Policy arithmetic is exact `Rational`. Conversion rates serialize exactly (`"1/3"` when the decimal does not terminate).

**Certificates must be reproducible.** No wall-clock time or randomness inside the kernel; intents carry their own timestamp. `palisade audit` re-derives every certificate byte for byte, so any change to certificate content or check wording changes hashes; treat it like a format change and rebuild the site.

**The kernel is pure.** IO lives in `Palisade.CLI` (and, later, servers and adapters). Every rejection is a typed value with an explanation.

**"Proof" wording.** Describe guarantees as type-enforced, unforgeable evidence plus checked laws. Never "formally verified". Make no claim of regulatory compliance.

**PolicyBench caveat.** `scenarios/policies/hardened.pal` was written alongside the incident corpus; its score shows what the language can express, not how well it generalises. Keep saying so wherever the score appears.

## Test Layout

Tests live in `test/`; `Gen.hs` has shared generators and fixtures, and `test/Palisade/*Spec.hs` mirror the modules. `TypeSafetySpec` uses `-fdefer-type-errors` and `-O0` to assert that ill-typed programs stay ill-typed; each ill-typed expression must be its own top-level binding or the deferred error fires when the spec is built.

## Toolchain

GHC 9.6.7 and cabal 3.10+ (CI pins cabal 3.10.3.0). `flake.nix` supports `nix build`, `nix run` and `nix develop`; `shell.nix` is for non-flake `nix-shell`.
