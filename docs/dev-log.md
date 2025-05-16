# RVRS Developer Log
---

## 🗓️ 2025-05-15 — Milestone Reached: v0.6.0-alpha

### ✅ Summary
- Merged `type-anno-dev` into `main` after full test suite passed
- Tagged `v0.6.0-alpha` as the stable foundation of RVRS
- Deleted old branches: `type-anno-dev`, `safe-base-v052`
- Confirmed all 20 tests:
  - ✅ 15 passed
  - ⚠️ 5 failed as expected
  - ❌ 0 unexpected failures
- Clean file structure under `tests/`
- All `-- expect-fail` logic handled by `RunAll.hs`
- `main` now reflects a clean, auditable, and production-ready alpha

### 🧠 Key Features in This Milestone
- Type annotations (e.g. `delta x: Num = 5`)
- Full test infrastructure with expected failure support
- Structured flow logic: branching, return, scope enforcement
- Boolean logic, assertions, mouth/whisper tested
- Preparatory groundwork for `v0.7.0`: ready for imports & IR

### 🧹 Repo Cleanup
- Local and remote branches removed:
  - `type-anno-dev`
  - `safe-base-v052`
- Hard reset synced local `main` to match forced push state
- Symbolic commit added to mark milestone

### 🧭 Next Steps (v0.7.0-dev)
- Begin `imports` syntax and implementation
- Sketch intermediate representation (IR) structure
- Add contract-mode flags/hooks (e.g. `@onchain`, `@mint`)
- Optional: begin stdlib structure or `use` system

> "🔖 Tag anchored, branches cleaned — v0.6.0-alpha stands stable and flowing."

---

## 🧭 v0.5.0-alpha – Flow Arguments & Expression Calling  
**📅 Date:** 2025-05-06  
**🔖 Tag:** v0.5.0-alpha

✅ Summary:
Flows can now take arguments and return values. Expressions can call flows directly (e.g. `call myFlow(1, 2)`). This unlocks real composability between flows and introduces full test coverage for argument binding, expression math, shadowing, branching, and flow result usage.

🔨 Core Changes:
- AST updated to support `CallExpr name [Expr]`
- Parser supports flow calls in both statement and expression form
- Evaluator binds argument values and executes in scoped environments
- Return values from flows can now be used in expressions (e.g., `source result = call myFlow()`)

🧪 Tests:
- ✅ `arg_bind_test.rvrs`
- ✅ `full_test.rvrs`

🎯 Next Goals:
- Type annotation enforcement
- Argument count validation errors
- Standard library flows: `bless`, `mirror`, `curse`


---

## 🌀 v0.4.0-alpha — The River Speaks  
**Date:** 2025-05-03  
**Tag:** v0.4.0-alpha

RVRS now supports **poetic delta syntax** — allowing declarations like:

```rvrs
delta silence "The river asks no questions"
```

No equals sign required. This unlocks a new expressive mode for writing RVRS flows in symbolic, lyrical form.

### 🔧 Changes:
- `deltaParser` now accepts both `delta x = ...` and `delta x "..."` forms
- Poetic flows now fully parse and evaluate
- `.gitignore` updated to cleanly exclude build artifacts

### ✅ Verified:
- `examples/poetic/rvrs.rvrs`: poetic flow chain
- `examples/full_test.rvrs`: full feature regressiong


---

## 🧭 v0.3.5-alpha — Stable Parser + Full Flow Execution

**Date:** 2025-05-01  
**Tag:** `v0.3.5-alpha`

✅ **Today’s Achievements:**
- Isolated and fixed critical `delta` parsing bug (`unexpected 'd'`)
- Implemented `NumLit Double` + `parseNumber` to support both `1` and `1.0`
- Verified parser is wired into `Main.hs` with debug trace
- Validated all core features with `full_test.rvrs`:
  - Flow definitions
  - `source` and `delta`
  - `echo`, `mouth`, `return`
  - Nested flow calls via `call` and `CallExpr`
  - Scoped environments + shadowing
  - Branch logic (`branch ... else ...`)
  - Arithmetic expressions (+ - * /)
- Cleaned `.cabal` and archived legacy `MiniParser`

🔖 Tagged: `v0.3.5-alpha` — first stable public-ready snapshot.

---

**Next Steps (Planned for v0.3.6+):**
- 🧠 Add support for flow **arguments** (e.g., `flow greet(name: Text)`)
- 🔁 Implement **loops or recursion** (starting with manual tail call)
- 🧪 Add **unit test file runner** (like `rvrs test/`)
- 🧰 Begin prepping **codegen or Aiken output**
- 🏗 Optional: add **type checking / validation**

---
## 🧭 v0.3.1-alpha – Scoped Branches & Variable Shadowing
**Date:** 2025-04-30

**Tag:** `v0.3.1-alpha`

✅ Summary:
RVRS now supports lexical scoping inside branches, including proper variable shadowing.

🔍 Scope Achievements:
Variables defined inside branch blocks are scoped locally

Variables declared outside remain unaffected after branch exit

Shadowing (re-declaring the same variable name) behaves correctly

Confirmed with multiple passing tests

🔬 Verified Tests:
scope_test.rvrs: confirms isolation of variables in branches

shadowing_test.rvrs: confirms that inner x does not overwrite outer x

---

## 🧭 v0.3.0-alpha – Multi-Flow River Paths  
**Date:** 2025-04-28  
**Tag:** `v0.3.0-alpha`

### ✅ Summary:
RVRS now supports multiple `flow` blocks per file, with a formal entrypoint flow (`main`).  
Flows can call one another using `call <name>`, allowing modular logic structures.

### 🔨 Core Changes:
- Parser now accepts many flows from a single source file.
- `evalFlow` updated to receive a flow environment (`Map String Flow`) for linking.
- `Call` now exists as a **statement**, not an expression.
- Added support for empty argument lists in `flow()` declarations.
- `Main.hs` upgraded to build a flow map and run the `"main"` flow.

---

## 🧭 v0.2.0 – Flow Foundations  
**Date:** 2025-04-27  
**Tag:** `v0.2.0`

### ✅ Summary:
Core control flow in RVRS is now implemented and stable.  
The `mouth` keyword halts a flow immediately and optionally returns a value.

### 🔨 Core Changes:
- `Mouth` returns and halts the flow using `Returned` type.
- `evalBody` short-circuits on `Returned`.
- `Echo`, `Mouth`, and `Return` all unified under a clean result model.
- Added `formatVal` helper for consistent output.

---

## 🧭 v0.1.0 – First Watershed  
**Date:** 2025-04-25  
**Tag:** `v0.1.0`

### ✅ Summary:
Initial prototype of the RVRS interpreter.  
Supports basic expressions, flow blocks, echo statements, and evaluation.

### 🔨 Core Features:
- Parser and AST scaffolded with `Flow`, `Echo`, `Expr`, and simple arithmetic.
- Basic environment for variable handling with `source` and `delta`.
- First evaluator logic with runtime expression handling.
