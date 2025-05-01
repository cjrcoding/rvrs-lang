# RVRS Developer Log
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
