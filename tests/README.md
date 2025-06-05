# 🧪 RVRS Tests

RVRS uses categorized test files to validate parser, evaluation, type checking, and IR behavior.

---

## 🗂 Test Folder Structure

- **`core/`** – Standard functionality (scoping, arithmetic, source/delta correctness)
- **`poetic/`** – Language-ritual samples using the most symbolic features
- **`regression/`** – Bugs or behaviors we've fixed and want to lock in
- **`typecheck/`** – Type annotation pass/fail tests
- **`ir/`** – Focused on IR evaluation and behavior
- **`edge_cases/`** – Unusual or extreme examples to test robustness
- **`flows/`** – Higher-level use cases or flow interactions
- **`syntax/`** – Validates surface syntax parsing (e.g. `speaks`, `ceremony`, etc.)
- **`debug_test.rvrs`** – Freeform sandbox file

---

## 🧪 How to Run Tests

Use `cabal run RunAll` to run all test files.  
Or `cabal run rvrs <file>` to run a specific test manually.

---

## 🧱 Philosophy

If it's not tested, it's not real.  
All new features or syntax extensions must be accompanied by one or more test cases.

