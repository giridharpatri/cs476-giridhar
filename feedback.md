# Feedback for Homework 3

## Score: 19.7/20

1. -0.3%: One use of `var`.

---

# Feedback for Homework 2

## Score: 12.8/15

1. -0.3%: One use of `var`.
2. -0.4%: DSL does not support nested classes.
3. -1.5%: Project does not build.

---

# Feedback for Homework 1

## Score: 14/15

1. -0.3%: One use of `var`.
2. -0.7%: Logic gates are not *reusable* with different inputs.

```scala
// Example of gate reuse; assume this in your syntax
Assign(LogicGate("exampleGate"), ADD(MULTIPLY(v1, v2)))
Eval("exampleGate", Inputs(i1, i2))
Eval("exampleGate", Inputs(i3, i4)) // exampleGate reused
```

Is this possible in your DSL?

---
