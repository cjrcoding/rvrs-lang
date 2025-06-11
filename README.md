
<p align="center">
  <img src="https://raw.githubusercontent.com/cjrcoding/rvrs-lang/main/assets/rvrs-logo-clean.jpg" alt="RVRS Logo" width="600"/>
</p>

# 🌊 Rivers (RVRS)

<p align="center">
  <a href="#">
    <img src="https://img.shields.io/badge/Version-v0.9.0--alpha-blueviolet?style=for-the-badge" alt="RVRS Version Badge"/>
  </a>
</p>

**“Not all contracts are code. Some are rivers.”**

RVRS is an experimental smart contract language for Cardano.  
It treats code as ritual. Every contract is a flow: deliberate, symbolic, and evolving.

RVRS is written in **Haskell**, fully custom, and compiles to an Intermediate Representation (IR) designed for [Aiken](https://aiken-lang.org).  
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

### ✅ Built So Far (v0.9.0-alpha)

RVRS now supports typed declarations, scoped evaluation, branching, flow composition, and evaluation tracing.

#### 🌐 Language Features
- ✅ Core 6 syntax and flow-based semantics
- ✅ Expression parsing + arithmetic and logic ops  
- ✅ Flow-to-flow calls with argument passing  
- ✅ Typed `delta` and `source` declarations (`delta x: Num = 5`)  
- ✅ Scoped variables, shadowing, and purity by default  
- ✅ Echo-based return system and optional `mouth` tracing  
- ✅ Multi-flow files, `main` entrypoint, and standard rituals  

#### ⚙️ Runtime + Infra
- ✅ AST → IR lowering pipeline  
- ✅ IR evaluator with scoped flows and error tracking  
- ✅ Static typechecking of expressions and declarations  
- ✅ Source, IR, and typecheck test runners:
  - `RunAll`: full test suite  
  - `RunIRTests`: IR-level tests  
  - `TestTypeCheck`: type inference and type error tests  
- ✅ Standard library merging via `stdlib.rvrs`  
- ✅ Developer docs, guides, and module maps

---

### 🧪 Sample Flow

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

RVRS is designed to compile into [Aiken](https://aiken-lang.org), Cardano’s strongly typed smart contract language.

- **Structured Output**: Aiken gives RVRS a clear, functional target  
- **Faster Dev**: Easier than writing directly in Plutus Core  
- **Type Safety**: Aiken’s type system complements RVRS  
- **Ecosystem Fit**: Great dev tools and growing Cardano community

📂 See [`examples/transpilation`](./examples/transpilation) for sample flows and Aiken targets.

---

### 🔮 What’s Flowing Next

- [ ] Full expression-level type enforcement  
- [ ] Type-safe flow argument validation  
- [ ] Return type inference and enforcement  
- [ ] Traceable error logs and assertion flows  
- [ ] REPL mode for interactive rituals  
- [ ] Aiken backend integration  

---

### 📖 Documentation

- [Style Guide](./docs/rvrs-style-guide.md)  
- [Dev Log](./dev-log.md)  
- [Testing Guide](./docs/test-infra.md)  
- [Module Map](./docs/module-map.md)  
- [Contributing](./CONTRIBUTING.md)  

---

🎨 **Philosophy**

RVRS is a language of calm. A language of meaning.

- Symbolism over syntax  
- Readability as ritual  
- Clear evaluation and flow  
- Inspired by rivers, nature, and the act of naming  

👤 Created by Carlos Javier Rivera  
- [GitHub](https://github.com/cjrcoding)  
- [IMDb](https://www.imdb.com/name/nm7121880/)

<p align="center">
  <a href="./dev-log.md">
    <img src="https://img.shields.io/badge/Dev%20Log-View%20Here-blue?style=for-the-badge" alt="Dev Log Button"/>
  </a>
</p>
