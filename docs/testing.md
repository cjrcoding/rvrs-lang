# RVRS Testing Guide

Welcome to the test suite for RVRS — the Ritual Virtual River System. This guide outlines how tests are structured, where to find them, and how to run them.

---

## 📁 Folder Structure

All test files live under the `examples/` directory and are grouped by intent:

| Folder         | Purpose                                                  |
|----------------|----------------------------------------------------------|
| `core/`        | Fundamental language features (math, scope, delta, etc.) |
| `edge_cases/`  | Unexpected, invalid, or boundary-case behaviors           |
| `flows/`       | Flow invocation, arguments, return logic                  |
| `poetic/`      | Symbolic, expressive, or aesthetic RVRS patterns          |
| `regression/`  | Full integration and coverage tests (`full_test.rvrs`)    |

---

## ▶️ Running Tests

To run a single test file:

```bash
cabal run rvrs examples/core/math_test.rvrs
```

To run the full test suite:

```bash
cabal run RunAll
```

To run IR-level evaluations (internal flow logic):

```bash
cabal run RunIRTests
```

---

## 🧪 Test Format

Each `.rvrs` file is a complete script and must contain a `flow main()` declaration.

RVRS will automatically evaluate the `main` flow in each test.

Expected forms of output and diagnostics:

- `echo` → returns a value and halts flow
- `mouth` → prints/logs a value (non-halting)
- `assert` → enforces truth, halts on failure
- `-- expect-fail` → marks a test expected to fail (e.g., type error)

Example:

```rvrs
flow main(): Num {
  delta x: Num = 4 * 2
  assert x == 8
  echo x
}
```

---

## ⚠️ Failure Reporting

- Type mismatches, undefined names, and assertion failures produce error messages
- Expected failures are tracked by `RunAll.hs` and `RunIRTests.hs`
- If a file marked `-- expect-fail` passes, the test fails intentionally
- If a file not marked `-- expect-fail` fails, the test fails genuinely

---

## ✅ Best Practices

- Group tests logically and name descriptively
- Use `assert` to verify correctness
- Always include a `main` flow
- Prefer `echo` or `mouth` for clarity
- Mark failing tests explicitly with `-- expect-fail`

---

> "Tests in RVRS aren’t just checks. They’re affirmations — that each branch splits, each echo returns, and each flow holds."

**Last Updated: 2025-05-22**
