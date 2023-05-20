# Simple derivation machine for simple expressions in Ocaml

This is a simple command-line application that parses simple expressions and computes a derivative with respect to a given variable.

This project was made so that I could practice with Ocaml and implementing recurse descent parsing. For simplicity, all of the source code for the application is in one single file.

## How to run 

- Use the Ocaml interpreter 

``` sh
ocaml derivative.ml
```

- Follow the instructions of the prompts!

## Examples of expressions

> x ** 2 * y

> x + y

> thing / otherThing -foo

> -x

## Syntax

For now, the differentiable expressions are compositions the following operations (with the following syntax) :

- Sum : **'+'**
- Product : **'*'**
- Difference : **'-'**
- Negation : **'-'** (unary operator)
- Division : **'/'**
- Exponentiation : **'\*\*'**

### **Ocaml rocks!**
