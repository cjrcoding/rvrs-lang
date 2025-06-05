<p align="center">
  <img src="https://raw.githubusercontent.com/cjrcoding/rvrs-lang/main/assets/rvrs-logo-clean.jpg" alt="RVRS Logo" width="600"/>
</p>

# 🌊 Rivers (RVRS)

<p align="center">
  <a href="#">
    <img src="https://img.shields.io/badge/Version-v0.8.4--alpha-blueviolet?style=for-the-badge" alt="RVRS Version Badge"/>
  </a>
</p>

**“Not all contracts are code. Some are rivers.”**

RVRS is an experimental smart contract language for Cardano.  
It treats code as ritual. Every contract is a flow: deliberate, symbolic, and evolving.

RVRS is written in **Haskell**, fully custom, and aims to compile into [Aiken](https://aiken-lang.org) once the interpreter is complete.  
It is still forming. Still flowing.

---

### ✨ The Core 6

These elemental constructs shape every ritual:

- `flow`: A container of purpose and invocation  
- `source`: A declaration of origin, a variable spring  
- `delta`: A transformation, a shift in state  
- `branch`: A decision point — a fork in the current  
- `mouth`: A voice — the moment the contract speaks  
- `echo`: A return, the final reflection  

These are the riverbed. The rest flows from here.

---

### ✅ Built So Far (May 2025)

You can now write full-featured RVRS flows with arguments, branching logic, and return values.

#### 🌐 Language Features
- ✅ Full parser for all Core 6 constructs  
- ✅ Arithmetic expression support: `+`, `-`, `*`, `/`  
- ✅ Logical comparisons: `==`, `!=`, `<`, `>`, etc.  
- ✅ Variable state mutation via `delta`  
- ✅ Scoped variables and shadowing behavior  
- ✅ Function-style `flow` blocks with arguments  
- ✅ Flow-to-flow calling using `call`  
- ✅ `echo` used as return value from flows  
- ✅ `mouth` emits trace logs  
- ✅ Multi-flow file support with `main` as entrypoint  

#### ⚙️ Runtime + Infrastructure
- ✅ Full interpreter with scoped evaluation  
- ✅ Intermediate Representation (IR) and evaluator  
- ✅ Type annotations on `delta` and `source`  
- ✅ CLI for `.rvrs` file execution and IR visualization  
- ✅ Standard library merging (`stdlib.rvrs`)  
- ✅ Test runners: `RunAll`, `RunIRTests`  
- ✅ Flow return control + runtime error detection

---

### 🧪 Real Example: Now Running

```rvrs
flow giveDiscount {
  return 0.1
}

flow computeTotal {
  source base = 100
  delta discount = call giveDiscount
  source total = base - (base * discount)
  mouth "start"
  echo total
}
```
```
mouth: start  
echo: 90.0
```

---

### 🔁 Why Aiken First?


RVRS is designed to compile into [Aiken](https://aiken-lang.org), a functional smart contract language for Cardano known for its strong typing and clean syntax.

Why not compile directly to Plutus Core?

- **Faster Development:** Aiken provides a modern, structured target that mirrors many of RVRS's core ideas.
- **Safer Code:** Aiken handles type inference and safety checks, which RVRS can leverage during codegen.
- **Readable Output:** Compiling to Aiken lets developers audit and learn from the generated contracts.
- **Community-Backed:** Aiken has active tooling and a growing ecosystem—ideal for early RVRS users.

Long-term, RVRS may support multiple backends (including direct Plutus Core). But Aiken offers the best foundation today.

📂 See [`examples/transpilation`](./examples/transpilation) for hand-written example flows and their expected Aiken output.



### 🔮 What’s Flowing Next

Coming soon to RVRS:

⏳ Type checking for flow arguments

⏳ Full expression-level type enforcement

⏳ Better error messages + flow tracing

⏳ Companion test flows and simulations

⏳ REPL mode (ritual scripting, live evaluation)

⏳ Aiken backend compiler integration

---


🎨 Design Philosophy
RVRS is more than syntax. It’s a style of thinking.

Ritual language and sacred geometry

Symbolic, readable code with intent

Calm aesthetic — designed for clarity and reflection

Inspired by nature, myth, and meaning

👤 Created by Carlos Javier Rivera

Actor. Writer. Builder.

- [GitHub: @cjrcoding](https://github.com/cjrcoding)  
- [IMDb](https://www.imdb.com/name/nm7121880/)

<p align="center"> <a href="./dev-log.md"> <img src="https://img.shields.io/badge/Dev%20Log-View%20Here-blue?style=for-the-badge" alt="Dev Log Button"/> </a> </p>
