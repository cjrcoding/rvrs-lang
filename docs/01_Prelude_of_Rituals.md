# 📜 Prelude of Rituals — RVRS Standard Library

Welcome to the **Prelude of Rituals**, the standard library of flows for the RVRS language. These are the first rites: composable, symbolic flows that extend the language’s power through its own poetic grammar.

They are written in pure RVRS, lowered to IR, and merged into the evaluation environment. This allows user flows to call them as if they were native constructs.

As of v0.9.0, these rituals are typechecked and executed in the same environment as user-defined flows.

---

## 🌊 Core Ritual Flows

```rvrs
flow mirror(x) {
  mouth x
}

flow void(x: Num) {
  mouth -x
}

flow fuse(x: Num, y: Num) {
  mouth x + y
}

flow is(x, y) {
  mouth x == y
}

flow fork(cond: Bool, a, b) {
  branch cond then {
    mouth a
  } else {
    mouth b
  }
}

flow echo_twice(x) {
  echo x
  echo x
}
```

---

## 🔤 Naming Philosophy

RVRS embraces a symbolic naming convention. Many flows use poetic or metaphorical language to reflect their function. Names may intersect with traditional programming terms, but are recontextualized through the lens of RVRS.

| Ritual Name | Meaning in RVRS              | Not to be confused with          |
|-------------|-------------------------------|----------------------------------|
| `mirror`    | Return the given value        | Identity function (generic)      |
| `void`      | Negate a number               | `void` type (C-like languages)   |
| `fuse`      | Add two numbers               | Haskell `Monoid` combine         |
| `is`        | Boolean equality check        | -                                |
| `fork`      | Choose between two flows      | Git forks, thread forks          |
| `echo_twice`| Echo a value twice            | -                                |

> ✨ These rituals are more than tools—they are **symbols**.

- They are also idioms. Examples of how to write expressive, idiomatic RVRS.

- See the [Style Guide](08_styleguide.md) for more on tone and structure.

---

## 🧪 Usage Example

These flows are directly invocable in any user-defined RVRS program:

```rvrs
flow main {
  delta a = 3
  delta b = 4
  echo fuse(a, b)        -- 7
  echo void(b)           -- -4
  echo is(a, a)          -- true
  echo fork(true, 1, 2)  -- 1
  echo fork(false, 1, 2) -- 2
  echo mirror("wave")    -- "wave"
}
```

---

## 🛠 Integration Notes

- File location: `stdlib/stdlib.rvrs`
- Loaded via parser and lowered to IR using `lowerFlow`
- Merged into the global `FlowEnv` during evaluation
- Flows defined here are testable via `RunIRTests`
- Flows defined here are validated by both the typechecker and evaluator.
- They are tested via `RunIRTests`, and merged automatically when `evalIRFlow` runs.

---

## 🌱 Next Steps

- Expand with additional rituals (e.g., `cleanse`, `align`, `multiply`, `greater_than`, `echo_if`)
- Begin annotating types for stricter validation
- Add control structures, utility flows, and symbolic patterns for contracts


🌟 Want to contribute a new ritual? See [07_contributions.md](07_contributions.md) and submit a symbolic flow.


---

### 🌀 The Prelude grows with the River.
Every new ritual is an offering to the current.
