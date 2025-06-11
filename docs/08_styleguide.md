# RVRS Style Guide

## 1. Code Formatting

### 1.1 Indentation & Layout
- Use 2-space indentation.
- Separate logical blocks with vertical spacing.
- Prefer line breaks for `case`, `do`, and `let` blocks when nested.

### 1.2 Imports
- Group imports: standard library first, then internal modules, then qualified imports.
- Example:
  ```haskell
  import Data.Map (Map)
  import qualified Data.Map as Map
  import RVRS.AST
  import Ya (Recursive(..), unwrap)
  ```

### 1.3 Language Extensions
- Declare extensions per module.
- Commonly used: `PatternSynonyms`, `StandaloneDeriving`, `UndecidableInstances`.

---

## 2. Naming Conventions

### 2.1 Modules
- Use `CamelCase`, namespaced under `RVRS.*`.

### 2.2 Types & Constructors
- Prefix concrete types with `T`: `TNum`, `TBool`.
- Use descriptive names: `FlowIR`, `EvalIR`, `EvalError`.

### 2.3 Functions
- Prefer short but descriptive names: `evalExpr`, `typeOfExpr`, `add`, `notExpr`.
- Use `expr` and `stmt` as common suffixes for handlers.

---

## 3. Expression & Evaluation Idioms

### 3.1 Pattern Matching with Recursive
- Always pattern match via `unwrap expr`.
- Avoid deeply nested expressions by breaking into smaller helpers.

### 3.2 Eval and Typecheck Structure
- Use monads for expression evaluation.
- Type errors are specific and custom: `TypeMismatch`, `UnknownVariable`, `UnsupportedOp`.

### 3.3 Binary Operations
- Use `checkBinary` and `binOp` helpers for consistency.

### 3.4 Error Handling
- Favor `throwError` over `fail` or `error` in monadic contexts.
- Avoid catching general exceptions; prefer typed control (`ReturnValue`, `RuntimeError`).

---

## 4. Testing Philosophy

### 4.1 Unit Tests
- Use `Test.HUnit` for typechecking logic.
- Prefer pattern coverage over random cases.

### 4.2 Integration & Regression
- Use `RunAll.hs` to scan, run, and summarize `.rvrs` files.
- Expected failures: detect via `-- expect-fail` comment.

### 4.3 Output Style
- Keep tests colorful, labeled, and expressive.

---

# Appendix A: RVRS Idioms

- **Recursive Wrapper**: Always use `Recursive` to wrap AST nodes.
- **Echo, Mouth, Whisper**: Reflective naming evokes a ritual tone. Do not replace with generic terms.
- **Delta vs Source**: `Source` is immutable (once set), `Delta` is mutable. Keep this metaphor consistent.

**Example:**
```haskell
Delta "x" Nothing (Recursive (NumLit 42))
Mouth (Recursive (Var "x"))
```

---

# Appendix B: Design Philosophy & Tone

- RVRS is not just a language — it is a **ritual system** for logic.
- It favors readability, expressiveness, and symbolic meaning.
- Output is part of the language’s *aesthetic contract*. It should feel alive and intentional.

### Naming Tone
- Prefer names that suggest meaning, action, or transformation: `Flow`, `Echo`, `Mouth`, `Branch`, `Delta`.

### Code as Ritual
- A flow defines intent.

- A statement expresses it.

- RVRS is written to resonate: read aloud, rendered with clarity, and shaped to feel alive.


