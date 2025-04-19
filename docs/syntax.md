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

## 🧪 Future Visual Language Ideas (Design Notes)
RVRS may later support a well-defined aesthetic to enhance readability and expression:

- 🎨 **Color Palette:**
  - Each keyword group (Core, Symbolic, Structural) could have a dedicated hue
  - E.g. `flow` = river blue, `branch` = moss green, `echo` = silver or mirror tones

- 🧱 **Visual Themes:**
  - Flows are structured like poems or spells — their formatting should reflect ritual spacing
  - Syntax highlighting or editor themes should reflect calm, natural elements (stone, ink, water)

- 📘 **Glyphs/Icons (Documentation only):**
  - Minimal, symbolic glyphs (not emojis) may be used in docs or READMEs to visually anchor concepts
  - Should be used sparingly to avoid overwhelming the core tone

- ⚙️ **Terminal Style (Optional):**
  - CLI might later support ANSI-colored output for statement types or errors, inspired by natural tones

These visuals are not required for compiling or authoring — but they serve to deepen the emotional resonance of the language.

---

### Last Updated: April 17, 2025
