

<p align="center">
  <img src="https://raw.githubusercontent.com/cjrcoding/rvrs-lang/main/assets/rvrs-logo-clean.jpg" alt="RVRS Logo" width="600"/>
</p>

# 🌊 Rivers (RVRS)

<p align="center">
  <a href="#">
    <img src="https://img.shields.io/badge/Version-v0.3.0--alpha-blueviolet?style=for-the-badge" alt="RVRS Version Badge"/>
  </a>
</p>



**“Not all contracts are code. Some are rivers.”**

RVRS is an experimental smart contract language for Cardano.
It treats code as ritual. Every contract is a flow: deliberate, symbolic, and evolving.

RVRS compiles to [Aiken](https://aiken-lang.org).  
It is still forming. Still flowing.

---

## ✨ The Core 6

These elemental constructs shape every ritual:

- `flow`: A container of purpose and invocation  
- `source`: A declaration of origin, a variable spring  
- `delta`: A transformation, a shift in state  
- `branch`: A decision point — a fork in the current  
- `mouth`: A voice — the moment the contract speaks  
- `echo`: A return, the final reflection

These are the riverbed. The rest flows from here.

---

## ✅ Built So Far (April 2025)

You can now write real, conditional, branching RVRS flows with computation and state.

- ✅ Full parser for `flow`, `delta`, `branch`, `mouth`, `echo`, and expressions  
- ✅ Arithmetic expression support: `+`, `-`, `*`, `/`  
- ✅ Variable bindings and state mutation via `delta`  
- ✅ Logical evaluation in `branch` blocks  
- ✅ `mouth` logs the trace of the contract  
- ✅ Pretty-printed AST output for debugging and clarity  
- ✅ CLI interface for `.rvrs` execution and interpretation

---

## 🧪 Real Example (Now Possible)

```rvrs
flow full_test {
  delta x = 5
  delta check = (x == 5)
  mouth "start"
  branch check {
    echo "matched"
    delta x = 10
  } else {
    echo "did not match"
  }
  echo "done"
}
```
```rvrs
Output:

mouth: start  
echo: matched  
echo: done
```

🔮 What’s Flowing Next
Coming soon to RVRS:

- `echo` returns as meaningful output from a flow

- Function-style flows with arguments and scoped return

- Nested branch logic

- Improved type safety and runtime checks

- Support for multi-flow files and contracts

- Companion test flows and demo contracts

🎨 Design Philosophy
RVRS is more than syntax. It’s a style of thinking.

Ritual language and sacred geometry

Readable, symbolic code with intent

Calm aesthetic — designed for clarity and reflection

Inspired by nature, myth, and meaning

This is a language with soul.

---

## 👤 Created by Carlos Javier Rivera  
*Actor. Writer. Builder.*

<p align="center">
  <a href="./dev-log.md">
    <img src="https://img.shields.io/badge/Dev%20Log-View%20Here-blue?style=for-the-badge" alt="Dev Log Button"/>
  </a>
  
- [GitHub: @cjrcoding](https://github.com/cjrcoding)  
- [IMDb](https://www.imdb.com/name/nm7121880/)

