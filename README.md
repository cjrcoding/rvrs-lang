

<p align="center">
  <img src="https://raw.githubusercontent.com/cjrcoding/rvrs-lang/main/assets/rvrs-logo-clean.jpg" alt="RVRS Logo" width="600"/>
</p>

# 🌊 Rivers (RVRS)

<p align="center">
  <a href="#">
    <img src="https://img.shields.io/badge/Version-v0.7.1--alpha-blueviolet?style=for-the-badge" alt="RVRS Version Badge"/>
  </a>
</p>


**“Not all contracts are code. Some are rivers.”**

RVRS is an experimental smart contract language for Cardano.
It treats code as ritual. Every contract is a flow: deliberate, symbolic, and evolving.

RVRS compiles to [Aiken](https://aiken-lang.org).  
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
- ✅ CLI tool for `.rvrs` execution and AST visualization  

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
````

````Output:
mouth: start  
echo: 90.0
````

🔮 What’s Flowing Next
Coming soon to RVRS:

- Type annotations on flow parameters (e.g. x: `Num`)

- Flow selection and invocation by name (like `flow main`)

- Companion test flows for validation and simulation

- Better runtime error reporting and validation

- Aiken backend compiler integration

🎨 Design Philosophy
RVRS is more than syntax. It’s a style of thinking.

Ritual language and sacred geometry

Readable, symbolic code with intent

Calm aesthetic — designed for clarity and reflection

Inspired by nature, myth, and meaning.

---

## 👤 Created by Carlos Javier Rivera  
*Actor. Writer. Builder.*

<p align="center">
  <a href="./dev-log.md">
    <img src="https://img.shields.io/badge/Dev%20Log-View%20Here-blue?style=for-the-badge" alt="Dev Log Button"/>
  </a>
  
- [GitHub: @cjrcoding](https://github.com/cjrcoding)  
- [IMDb](https://www.imdb.com/name/nm7121880/)

