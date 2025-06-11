# 🌊 RVRS Module Map

This document outlines the core modules of **RVRS** and their responsibilities.  
Use this map to understand how code flows through the system.

---

## 📦 Core Modules

| Module             | File                      | Purpose |
|--------------------|---------------------------|---------|
| `RVRS.Parser`      | `src/RVRS/Parser/`        | Parses RVRS source into AST form. Submodules handle expressions, statements, types, and imports. |
| `RVRS.AST`         | `src/RVRS/AST.hs`         | Defines the high-level abstract syntax tree used by the parser. |
| `RVRS.Lower`       | `src/RVRS/Lower.hs`       | Converts AST to IR (intermediate representation). |
| `RVRS.IR`          | `src/RVRS/IR.hs`          | Defines intermediate representation used for evaluation and type checking. |
| `RVRS.Typecheck.*` | `src/RVRS/Typecheck/`     | Static type system logic, including `Check`, `Types`, and helpers. |
| `RVRS.Env`         | `src/RVRS/Env.hs`         | Runtime environment definitions and variable bindings. |
| `RVRS.Value`       | `src/RVRS/Value.hs`       | Runtime value definitions (`VStr`, `VNum`, etc). |
| `RVRS.Eval.*`      | `src/RVRS/Eval/`          | IR evaluation logic. Split into `Expr`, `Stmt`, and `Flow`. |
| `RVRS.Codegen`     | `src/RVRS/Codegen.hs`     | (Planned) Code generation target (e.g., Aiken). |
| `RVRS.Pretty`      | `src/RVRS/Pretty.hs`      | Pretty-printing for AST, values, and diagnostics. |
| `Ya.Recursive`     | `lib/Ya/Recursive.hs`     | Integrated support for recursive types via `Ya`. |

---

## 🧪 Testing Modules

| Module             | File(s)                  | Purpose |
|--------------------|--------------------------|---------|
| `Main.hs`          | `app/Main.hs`            | Entry point (future REPL or CLI tool). |
| `RunAll.hs`        | `app/RunAll.hs`          | Runs the full test suite. |
| `RunIRTests.hs`    | `app/RunIRTests.hs`      | Evaluates IR-level logic. |
| `TestLower.hs`     | `app/TestLower.hs`       | Verifies correctness of AST → IR lowering. |
| `TestTypeCheck.hs` | `app/TestTypeCheck.hs`   | Unit tests for expression-level type inference. |

---

## 🧱 Stdlib and Rituals

| File                     | Purpose |
|--------------------------|---------|
| `stdlib/stdlib.rvrs`     | Core prelude flows (standard rituals). |
| `docs/Prelude_of_Rituals.md` | Documentation for the standard library. |

---

## 📁 Directory Layout Summary

- `src/` – Source code for the language
- `lib/` – External integrations (e.g., `Ya`)
- `app/` – Entry points and test runners
- `examples/` – Flow scripts and test cases
- `docs/` – Documentation and reference
- `stdlib/` – Built-in RVRS flows and rituals

---

> “The modules of RVRS reflect the same structure as its syntax: a layered current — flowing from ritual to result.”
