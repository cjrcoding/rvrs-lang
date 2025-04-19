# 📜 RVRS Syntax Reference

> "Not all contracts are code. Some are rivers."

This document defines the evolving syntax of RVRS — a ceremonial language for writing smart contracts on Cardano.

---

## 🌿 Core Keywords (Minimum Viable Ritual)
These are essential for writing any valid RVRS `flow`:

| Keyword   | Meaning                        | Aiken Equivalent |
|-----------|---------------------------------|------------------|
| `flow`    | Function definition / ritual    | `fn`             |
| `source`  | Bind a value                    | `let`            |
| `delta`   | Reassign / update a value       | `let`            |
| `branch`  | Conditional block               | `if` / `else`    |
| `mouth`   | Output / log a value            | `trace`          |
| `echo`    | Return a value from the flow    | `return`         |

---

## ✨ Symbolic Keywords (Poetic & Expressive)
These enhance meaning and provide expressive ritual structure:

| Keyword     | Meaning                                | Aiken Mapping / Role |
|-------------|-----------------------------------------|----------------------|
| `veil`      | Hide, guard, or reveal a truth          | `Maybe`, `match`     |
| `invoke`    | Call another flow                       | function call        |
| `tide`      | Iterate over a list or sequence         | loop / stream        |
| `stream`    | Represents a flowing list               | list structure       |
| `bless`     | Symbolic permission or action trigger   | user-defined         |
| `chant`     | Symbolic evaluation / transformation    | expression wrapper   |
| `glyph`     | Declare a symbolic or reusable type     | `type`               |
| `ritual`    | Define reusable flow/module             | module / type        |

---

## 🪨 Structural & Meta Keywords (Constants & Control)
These control flow outside the core runtime — like constants, errors, or structure.

| Keyword     | Meaning                              | Aiken Equivalent  |
|-------------|---------------------------------------|-------------------|
| `pillar`    | Immutable constant                    | `const`           |
| `mark`      | Annotate or tag a declaration         | (not yet mapped)  |
| `drift`     | Exit or fail early                    | `fail`            |
| `mouthpiece`| Conditional trace/output              | `trace_if`        |
| `echo_if`   | Conditional return                    | n/a               |

---

## 🔮 Notes & Style
- All keywords are lowercase, symbolic, and evocative.
- RVRS prioritizes flow, readability, and poetic clarity over minimalism.
- New keywords are only added if they bring expressive or functional clarity.

---

### Last Updated: April 17, 2025
