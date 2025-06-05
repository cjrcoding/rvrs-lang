# Introduction to RVRS

RVRS (Rivers) is a symbolic smart contract language designed for the Cardano blockchain. It treats code not just as logic, but as ritual—something expressive, structured, and meaningful.

This introduction explains the "why" of RVRS before the "how."

---

## 🌊 What Is RVRS?

RVRS is a domain-specific language (DSL) for writing smart contracts in a symbolic, ritualistic style. Every contract is a flow: deliberate, readable, and expressive. It emphasizes clarity over cleverness and intention over terseness.

RVRS compiles into [Aiken](https://aiken-lang.org), a Cardano smart contract language. This allows symbolic RVRS contracts to execute safely on-chain.

---

## ✨ Why Symbolic?

Traditional languages focus on low-level precision. RVRS embraces symbolic constructs that:

- Reflect intent directly (`delta` for change, `branch` for decision)
- Encourage poetic expression in logic
- Help developers write with clarity, rhythm, and flow

It's a language where the *meaning* of code is as important as its execution.

---

## 🔁 Current Capabilities

As of version `v0.8.5`, RVRS supports:

- Full parser for all Core 6 constructs
- Intermediate Representation (IR) + interpreter
- Type annotations for variables
- Flow evaluation with return and trace
- Early Aiken transpilation examples (see `/examples/transpilation`)

Upcoming milestones include static type checking, codegen, and developer tooling.

---

## 🌀 Reading Flow

To understand RVRS in depth, follow this order:

1. **[Introduction](./00_intro.md)** – You are here  
2. **[Roadmap](./02_roadmap.md)** – See what’s coming  
3. **[Syntax](./03_syntax.md)** – Understand the Core 6 and expression rules  
4. **[Testing](./04_testing.md)** – Learn how we verify RVRS behavior  
5. **[Module Map](./05_module-map.md)** – Explore the codebase structure  
6. **[Developer Log](./06_dev-log.md)** – View progress and recent changes

---

RVRS is still forming. Still flowing. But it invites you to build something meaningful—ritual by ritual, line by line.
