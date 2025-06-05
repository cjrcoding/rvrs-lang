# 🌊 RVRS Module Map

This document outlines the core modules of RVRS and their responsibilities. Use this map to understand how code flows through the system.

---

## 📦 Core Modules

| Module | File | Purpose |
|--------|------|---------|
| `RVRS.Parser` | `src/RVRS/Parser/` | Parses RVRS source into AST form. Submodules handle expressions, statements, types, and imports. |
| `RVRS.AST` | `src/RVRS/AST.hs` | Defines the high-level abstract syntax tree used by the parser. |
| `RVRS.Lower` | `src/RVRS/Lower.hs` | Converts AST to IR (intermediate representation). |
| `RVRS.IR` | `src/RVRS/IR.hs` | Defines intermediate representation used for evaluation and type checking. |
| `RVRS.TypeCheck` | `src/RVRS/TypeCheck.hs` | Performs static analysis and ensures type correctness of IR. |
| `RVRS.Env` | `src/RVRS/Env.hs` | Runtime environment definitions and variable bindings. |
| `RVRS.Value` | `src/RVRS/Value.hs` | Core runtime value definitions (`VStr`, `VNum`, etc). |
| `RVRS.Eval.*` | `src/RVRS/Eval/` | Evaluation of IR constructs. Split into `Expr`, `Stmt`, and `Flow` evaluators. |
| `RVRS.Codegen` | `src/RVRS/Codegen.hs` | (Scaffolded) Future code generation target (e.g., Aiken). |
| `RVRS.Pretty` | `src/RVRS/Pretty.hs` | Pretty-printing for diagnostics or output. |

---

## 🧪 Testing Modules

| Module | File(s) | Purpose |
|--------|---------|---------|
| `Main.hs` | `app/Main.hs` | Entry point (placeholder or REPL, if enabled). |
| `RunAll.hs` | `app/RunAll.hs` | Runs the full test suite. |
| `RunIRTests.hs` | `app/RunIRTests.hs` | Runs IR-focused tests. |
| `TestLower.hs` | `app/TestLower.hs` | (Optional) Verifies lowering correctness. |

---

## 🧱 Stdlib and Rituals

| File | Purpose |
|------|---------|
| `stdlib/stdlib.rvrs` | Core prelude flows (standard rituals). |
| `docs/Prelude_of_Rituals.md` | Documentation of the stdlib symbols. |

---

## 📁 Directory Layout Summary

- `src/`: Source code
- `tests/`: Test files grouped by purpose
- `docs/`: Project documentation
- `examples/`: Showcase or learning flows
- `app/`: Executables and test runners

