# RVRS Testing Guide

Welcome to the test suite for RVRS — the Ritual Virtual River System. This guide outlines how tests are structured, where to find them, and how to run them.

---

## Folder Structure

All test files live under the `examples/` directory and are grouped by intent:

| Folder            | Purpose                                             |
|-------------------|-----------------------------------------------------|
| `core/`           | Fundamental language features (math, scope, etc.)   |
| `edge_cases/`     | Unexpected or error-prone behaviors                 |
| `flows/`          | Multi-step flow logic and function call behavior    |
| `poetic/`         | Symbolic, expressive, or aesthetic RVRS syntax      |
| `regression/`     | Full-suite integration tests (`full_test.rvrs`)     |

---

## Running a Test

To run a single test file from the repo root:

```bash
cabal run rvrs examples/core/math_test.rvrs
````

To run the full integration test:

````bash
cabal run RunAll
````

## Running a Test

Each test file is a complete .rvrs script.

RVRS always runs the main flow in each file.

Output will appear in your terminal via:

echo → prints human-facing values

whisper → prints expression + value

mouth → returns a value and halts the flow

Failed assertions or unresolved values will print diagnostic messages.



