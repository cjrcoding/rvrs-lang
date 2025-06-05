# RVRS → Aiken Transpilation Examples

This folder contains early examples of how symbolic RVRS flows might compile into [Aiken](https://aiken-lang.org) smart contracts.

Note: These outputs are manually written for now. Once RVRS code generation is complete, these examples will be produced automatically.

---

## Examples

### Example 1: `double`

#### RVRS Input

```rvrs
flow double(x) {
  delta result = x + x
  mouth result
}
```

#### Expected Aiken Output

```aiken
fn double(x: Int) -> Int {
  let result = x + x
  result
}
```

This example illustrates the transformation of symbolic ritual into functional logic:
- `delta` → `let`  
- `mouth` → trace output  
- Structural clarity maintained

---

### Example 2: `checkEligibility`

#### RVRS Input

```rvrs
flow checkEligibility(age) {
  branch age >= 18 then {
    mouth "eligible"
    echo true
  } else {
    mouth "not eligible"
    echo false
  }
}
```

#### Expected Aiken Output

```aiken
fn check_eligibility(age: Int) -> Bool {
  if age >= 18 {
    log("eligible")
    true
  } else {
    log("not eligible")
    false
  }
}
```

This example shows symbolic conditional logic (`branch`) mapped to a standard `if/else` construct in Aiken. The intent remains readable and contract-safe.

---

### Example 3: `squareByDouble`

#### RVRS Input

```rvrs
flow double(x) {
  delta result = x + x
  echo result
}

flow squareByDouble(x) {
  delta result = call double(x)
  echo result
}
```

#### Expected Aiken Output

```aiken
fn double(x: Int) -> Int {
  let result = x + x
  result
}

fn square_by_double(x: Int) -> Int {
  let result = double(x)
  result
}
```

This example demonstrates:
- Flow-to-flow composition using `call`
- Clean mapping to nested function calls
- Continued use of `delta` and `echo` for state and output

---

### Example 4: `getGreeting` (Typed Source)

#### RVRS Input

```rvrs
flow getGreeting(name) {
  source message: Str = "Hello, " + name
  echo message
}
```

#### Expected Aiken Output

```aiken
fn get_greeting(name: String) -> String {
  let message: String = "Hello, " + name
  message
}
```

This example highlights:
- Type annotation on a `source` declaration
- Mapping from `Str` → `String`
- Type-safe variable binding and return structure

---

These examples represent the first steps toward code generation in RVRS. As the interpreter evolves, these files will serve as reference points for validating the compiler pipeline.

More to come: refined type handling, nested flows, and complete contract scaffolds.
