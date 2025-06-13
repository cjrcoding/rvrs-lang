# RVRS Roadmap

This document outlines the development roadmap for RVRS, a symbolic smart contract language for Cardano.

RVRS is evolving through versioned releases toward a stable, expressive, and secure contract-writing experience. This roadmap captures current progress, upcoming features, and long-term goals.

_Updated for version `v0.9.0`_

---

## ✅ Core Milestones (Completed)

| Version     | Feature                                                | Status      |
|-------------|--------------------------------------------------------|-------------|
| `v0.1`–`v0.6` | Parser + Core 6 constructs (`flow`, `delta`, `branch`, etc.) | ✅ Complete |
| `v0.7`       | Intermediate Representation (IR) + evaluation layer   | ✅ Complete |
| `v0.8`       | Type annotations + scoped variable enforcement        | ✅ Complete |
| `v0.8.5`     | Aiken target rationale + hand-written transpilation examples | ✅ Done     |
| `v0.9.0`     | Static typechecking + Ya integration + style guide    | ✅ Complete |

---

## 🔜 Near-Term Goals

| Target  | Feature                                     | Notes                     |
|---------|---------------------------------------------|---------------------------|
| `v1.0`  | First working code generation pass → Aiken  | MVP contract output       |
| `v1.1`  | Refined error messages + REPL mode          | Developer feedback tools  |
| `v1.2`  | Project scaffolding + CLI utilities         | `rvrs new`, `rvrs fmt`, `rvrs run` |

---

## 🌱 Future Language Features

| Category     | Feature              | Description                                       |
|--------------|----------------------|---------------------------------------------------|
| Typing       | Nominal Type System  | Default: Types are declared by name              |
| Typing       | Refinement Types     | Enforce conditions on data (e.g., "only if > 0") |
| Typing       | Indexed Types        | State-aware types (e.g., “only callable after deposit”) |
| Control Flow | Pattern Matching     | Symbolic deconstruction inside `branch` or `flow`|
| Modularity   | Imports & Modules    | Import other `.rvrs` flows and stdlib            |
| Contracts    | Entry Points & Params| On-chain execution constraints and validators     |
| Codegen      | Plutus Core Backend  | Direct-to-core alternative target                |
| Codegen      | JSON/Metadata Emit   | For off-chain metadata and Midnight DX use       |

---

## 🌀 Philosophy Goals

RVRS isn’t just a programming language — it’s a ritualistic and symbolic framework. These goals continue to shape its identity:

- ✍️ Develop a `guide`/`muse` **Spirit Layer** — a conversational AI co-authoring interface
- 📖 Finalize the `Founding Fibers` — RVRS’s guiding philosophy and governance model
- 🪞 Create a **visual syntax explorer** — a ritual map or AST tree grown from flows
- 📘 Maintain the [Style Guide](08_styleguide.md) — to codify RVRS idioms and tone

---

## 🧭 Long-Term Vision

- Write symbolic smart contracts in a poetic, expressive form
- Transpile to production-grade Aiken or Plutus Core
- Foster a new aesthetic and mental model for contract authorship
- Build tools for onboarding, ritual learning, and creative dev expression

---

For current progress, see [`README.md`](../README.md)  
Or explore examples in the [`examples/`](../examples/) directory.
