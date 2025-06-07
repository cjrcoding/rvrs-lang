# CONTRIBUTING TO RVRS

Thank you for your interest in contributing to RVRS. This project is a domain-specific language for writing smart contracts using symbolic, flow-based syntax. This guide outlines how to contribute code, report issues, and engage with the project effectively.

---

## Project Structure Overview

| Folder      | Purpose                                                              |
|-------------|----------------------------------------------------------------------|
| `src/`      | All core language logic: parsing, evaluation, IR, etc.              |
| `tests/`    | Test files categorized by theme (e.g., core, edge_cases, typecheck).|
| `app/`      | Executables for running tests (e.g., `RunAll`, `RunIRTests`).        |
| `docs/`     | Developer-facing documentation and module guides.                    |
| `stdlib/`   | Core standard library flows in RVRS syntax.                          |
| `examples/` | Sample flows for learning, demonstration, or debugging.              |

_See `docs/module-map.md` and `tests/README.md` for details._

---

##  How to Contribute

### 1. Set Up the Project

```bash
git clone https://github.com/YOUR_USERNAME/rvrs-lang.git
cd rvrs-lang
cabal build
cabal run RunAll  # Run all tests to verify setup
```

### 2. Create a Feature Branch

```bash
git checkout -b feature/my-change
```

### 3. Follow RVRS Design Patterns

- Use **constructor-driven pattern matching** for evaluation and parsing.
- Keep concerns isolated (e.g., parsing, type checking, evaluation).
- Follow naming conventions (`flow`, `delta`, `mouth`, etc.).
- Prefer clarity over cleverness.

---

### 4. Add Tests

All behavioral changes should include or update one or more test cases.

- Place tests in the appropriate `tests/` category (see `tests/README.md`).
- Use `echo` or return values to demonstrate expected behavior.
- Run tests with:

```bash
cabal run RunAll
cabal run rvrs tests/path/to/test.rvrs
```

> ** Testing Policy for Contributors**
>
> All behavioral changes should include or update one or more tests.  
> This includes:
> - New syntax or keywords  
> - Bug fixes or regression prevention  
> - Evaluation logic changes  
>
> If you're not sure how or where to write a test, open a draft PR or ask in a discussion—we’ll help guide it into the right category.  
>
> **Exceptions**:
> - Documentation-only changes  
> - Pure refactors with no logic change (if covered by existing tests)  
>
> Tests in RVRS are simple `.rvrs` flows using `echo`, `mouth`, or visible side effects.  
> If it’s worth building, it’s worth testing.

---

### 5. Commit Cleanly

Use clear commit messages and sign your commits if possible:

```bash
git commit -S -m "feat: add type inference for delta declarations"
```

Examples:

- `fix: correct scope resolution in EvalStmt`
- `test: add regression for return leaks`
- `docs: clarify IR module structure`

---

### 6. Open a Pull Request

After pushing your branch:

- Open a PR to `main`
- Describe what you changed and why
- Link related issues (e.g., `Fixes #7`, `Related to #5`)
- Expect code review before merge

---

## Contribution Checklist

- [ ] Code follows existing design patterns  
- [ ] New logic is tested  
- [ ] Commits are clear and signed  
- [ ] Docs updated if behavior changed  
- [ ] PR is scoped and focused  

---

## 🔍 Reporting Issues

When reporting a bug or proposing a feature, please include:

- A brief description of the issue or feature  
- Steps to reproduce (if applicable)  
- Expected vs actual behavior  
- Relevant `.rvrs` code or logs  

---

## Collaboration Culture

- Keep feedback constructive and focused  
- Use descriptive names and comments  
- Small, self-contained PRs are preferred  
- Respect the balance of symbolism and structure in RVRS  

---
