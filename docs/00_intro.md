# Introduction to RVRS

RVRS (Rivers) is a symbolic smart contract language designed for the Cardano blockchain. It treats code not just as logic, but as ritual — something expressive, structured, and meaningful.

This introduction explains the "why" of RVRS before the "how."

---

## 🌊 What Is RVRS?

RVRS is a domain-specific language (DSL) for writing smart contracts in a symbolic, ritualistic style. Every contract is a flow: deliberate, readable, and expressive. It emphasizes clarity over cleverness, and intention over terseness.

RVRS compiles into [Aiken](https://aiken-lang.org), a Cardano-native smart contract language. This allows expressive RVRS contracts to be executed safely on-chain.

---

## ✨ Why Symbolic?

Most programming languages are built for machines first and humans second. RVRS reverses this.

It embraces symbolic constructs that:

- Reflect intent directly (`delta` for change, `branch` for decision)
- Encourage poetic expression in logic
- Help developers write with rhythm, resonance, and clarity

It’s a language where the *meaning* of code matters as much as its behavior.

---

## 🔁 Current Capabilities

As of version `v0.9.0`, RVRS includes:

- ✅ Full parser for all Core 6 constructs
- ✅ Intermediate Representation (IR) and interpreter
- ✅ Static typechecking for expressions and flows
- ✅ Type annotations for variables
- ✅ Flow evaluation with return, trace, and control
- ✅ Ya-based recursive AST structure
- ✅ A growing test suite with golden + regression support

Upcoming milestones include improved REPL support, contract-mode flow targeting, and WebAssembly compilation.

---

## 🧘 Ritual Syntax & Style

RVRS code is meant to be read aloud — like invocation.

To maintain expressive clarity and consistency, we offer a full [Style Guide](./08_styleguide.md) covering idioms, formatting, and design tone.

---

## 🌀 Reading Flow

To understand RVRS in depth, follow this order:

1. **[Introduction](./00_intro.md)** – You are here  
2. **[Roadmap](./02_roadmap.md)** – See what’s coming  
3. **[Syntax](./03_syntax.md)** – Understand the Core 6 and expression rules  
4. **[Testing](./04_testing.md)** – Learn how we verify RVRS behavior  
5. **[Module Map](./05_module-map.md)** – Explore the codebase structure  
6. **[Developer Log](./06_dev-log.md)** – View progress and recent changes  
7. **[Style Guide](./08_styleguide.md)** – Learn how to write idiomatic RVRS

---

RVRS is still forming. Still flowing.  
But it invites you to build something meaningful — ritual by ritual, line by line.
