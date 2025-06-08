# 📜 RVRS Syntax Reference

> "Not all contracts are code. Some are rivers."

This is the living syntax document for **RVRS** — a ceremonial, expressive language for writing smart contracts on Cardano. It outlines the current capabilities (as of `v0.8.0-dev`) and previews functionality expected in the `v1.0` launch.

---

## 🌿 Core Rituals (Executable Essentials)

These are the minimum required to write working flows (functions) in RVRS:

| Keyword   | Meaning                             | Aiken Equivalent |
|-----------|--------------------------------------|------------------|
| `flow`    | Function definition / ritual         | `fn`             |
| `delta`   | Define or mutate a scoped variable   | `let` / rebind   |
| `source`  | Top-level constant definition        | `let` (const)    |
| `echo`    | Return a value from a flow           | `return`         |
| `mouth`   | Emit/log a value without halting     | `trace`          |
| `branch`  | Conditional block                    | `if` / `else`    |
| `return`  | (Alternative to `echo`, optional)    | `return`         |

---

## 🧠 Type System (Enforced Constructs)

RVRS supports static typing via annotations (enforced in `v0.8.0+`):

```rvrs
delta x: Num = 5
flow identity(n: Num): Num {
  echo n
}
```

Supported base types:
- `Num`
- `Bool`
- `Str` *(planned for v1.0)*
- `List[T]` *(planned)*

---

## 📦 Flow Composition (Calls & Modularity)

| Keyword   | Meaning                          | Role in Execution     |
|-----------|----------------------------------|------------------------|
| `call`    | Invoke another flow              | Function call          |
| `return`  | Exit with a value                | Return from flow       |
| `assert`  | Require truth or halt            | Assertion guard        |
| `import`  | Bring in external `.rvrs` flows  | Module inclusion (v1)  |
| `@onchain`, `@mint`, `@view` | Flow attributes | Contract modes (planned) |

---

## 🕊️ Symbolic & Expressive Keywords *(Experimental / Future)*

These are poetic or expressive tools that may evolve post-v1:

| Keyword     | Meaning                             | Mapping / Status     |
|-------------|--------------------------------------|-----------------------|
| `veil`      | Optional or hidden logic             | `Maybe` / planned     |
| `tide`      | Iterate over a sequence              | `for` / planned       |
| `stream`    | Represents a flowing list            | `List` / placeholder  |
| `chant`     | Symbolic transformation              | Expr macro?     |
| `glyph`     | User-defined symbolic type           | `type` / long-term    |
| `ritual`    | Named reusable block/module          | Planned               |

---

## 🪨 Structural & Meta Keywords

Used for constants, annotation, or symbolic structure.

| Keyword     | Meaning                              | Notes                  |
|-------------|---------------------------------------|------------------------|
| `pillar`    | Immutable constant                   | `const`                |
| `mark`      | Annotation or tag                    | Could support metadata |
| `drift`     | Halt flow early / fail               | `fail` / `error`       |
| `mouthpiece`| Conditional trace                    | Planned                |
| `echo_if`   | Conditional return                   | Internal sugar         |

---

## 🧪 Syntax & Style Notes

### 🎨 Visual Identity *(Design Layer — Optional)*

- **Syntax Themes:** River-blue for flows, moss-green for branches, ink-black for deltas
- **Poetic Formatting:** Flows should be readable like ritual text or spells — indented with space to breathe
- **CLI Aesthetic:** Terminal colors based on type or keyword class (e.g. `delta` = blue, `echo` = white)

### 🔖 Annotations & Contracts

- Planned decorator-style flags for `@onchain`, `@mint`, `@test`, `@view`
- Could extend into validator-type generation or behavior-based compilation paths

### 📘 Icons/Glyphs (Docs Only)

- Use symbolic icons sparingly (no emojis) to represent conceptually heavy constructs
- Example: `🌊` = `flow`, `🪨` = `pillar`, `🔀` = `branch` — docs only

---

### ✅ Current Status:  
RVRS v0.8.0-dev supports:
- Full `flow` and variable definition syntax
- Type annotations and basic enforcement
- IR evaluation and test infrastructure
- Standard library merging at runtime

---

> "RVRS is not just a language. It's a current — designed to carry intent with clarity, weight, and meaning."

**Last Updated: 2025-05-22**
