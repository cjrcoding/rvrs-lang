# RVRS Engine Tests

This is the **official test suite** for the `RVRS.Engine` evaluator, the recursive core of the RVRS language.

Each `.rvrs` file defines a top-level `flow main { ... }`, which is parsed, evaluated, and printed.

---

## 🗂 Folder Structure

- **`engine/`** – All tests built on the new `RVRS.Engine` recursive evaluator
- **`legacy/`** – Archived tests from earlier implementations (IR, Lower, old typechecker)

---

## How to Run

Run **all engine-based tests**:

```bash
cabal run runengine
```

Run **a specific file manually**:

```bash
cabal run rvrs tests/engine/basic_eval.rvrs
```

---

## Current Engine Tests

| File              | Purpose                               |
|-------------------|----------------------------------------|
| `basic_eval.rvrs` | Delta + Echo — minimal working sanity |
<!-- More tests will be added here -->

---

## 🧱 Testing Philosophy

> If it’s not tested, it’s not real.

RVRS treats tests as ritual and proof.

All new features — whether parser syntax, evaluator logic, or language constructs — **must be accompanied by tests** under the engine framework.  
We aim for clarity, confidence, and coverage at every layer.

---

## Legacy Tests (Archived)

We’ve moved all old tests into the `legacy/` folder, categorized as:

- `core/` – Arithmetic, scoping, variable logic
- `typecheck/` – Pass/fail tests for type enforcement
- `flows/` – Higher-level user flows
- `edge_cases/` – Unusual or extreme inputs
- `ir/` – Legacy IR-based evaluation
- `syntax/` – Validates surface syntax
- `regression/` – Historical bugs and their fixed outputs
- `poetic/` – Language-ritual tests using symbolic style
- `debug_test.rvrs` – A freeform sandbox

Legacy tests will not be deleted, but they will **not be maintained or run** unless ported to the new engine system.
