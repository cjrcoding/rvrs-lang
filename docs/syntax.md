# RVRS Syntax: Core Constructs

This document outlines the **core syntax** of RVRS — the Ritual Language for writing expressive smart contracts that compile to Aiken.

These keywords form the foundation of RVRS, enabling readable, ceremonial logic.

## ✨ Core Keywords

| Keyword | Purpose | Equivalent (Aiken) | Description |
|---------|---------|--------------------|-------------|
| `flow` | Declare a function or validator | `fn`, `validator` | The main block of logic, a ritual of action |
| `source` | Bind input or define a value | `let` | Establish origin of variables or parameters |
| `branch` | Conditional logic | `if / else` | Split logic based on condition, diverge the current |
| `delta` | Declare or update a variable | `let` (reassignment) | Marks change or transition |
| `mouth` | Emit a message or trace | `trace` | Ritual output — let the contract speak |
| `echo` | Return a value from the flow | `return` or final expr | The final whisper — output of the function |

## 📘 Sample Flow

```rvrs
flow grant_access(user: Identity) {
  source trust = check_trust(user)

  branch trust == truth {
    delta state = "allowed"
    mouth "Access granted."
  } else {
    mouth "Access denied."
    delta state = "denied"
  }

  echo state
}
