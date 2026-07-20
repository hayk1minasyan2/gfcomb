# GFComb

GFComb is a Haskell library and command-line tool for working with ordinary generating functions.

The project currently supports:

* formal power series over rational numbers;
* polynomial arithmetic;
* rational generating functions;
* generating functions derived from linear recurrences;
* predefined generating functions such as Fibonacci;
* a basic interactive REPL.

## Build

From the project root:

```bash
cabal build all
```

## Run tests

```bash
cabal test
```

## Run the REPL

```bash
cabal run gfcomb
```

## Available REPL commands

```text
help
list
show NAME
quit
exit
```

Example:

```text
gfcomb> list
Available predefined generating functions:
  fibonacci

gfcomb> show fibonacci
Name: fibonacci
Description: Fibonacci numbers: 1, 1, 2, 3, 5, 8, ...
Generating function: 1 / (1 - x - x^2)
First 10 coefficients: [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

## Project structure

```text
app/REPL.hs                 Interactive command-line interface
src/GFComb/Core.hs          Formal power series
src/GFComb/Polynomial.hs    Polynomial operations
src/GFComb/RationalGF.hs    Rational generating functions
src/GFComb/Recurrence.hs    Linear recurrence support
src/GFComb/Builtins.hs      Predefined generating functions
test/Main.hs                Tests
```

## Current status

The current implementation provides the basic library structure, linear recurrence support, Fibonacci as a predefined generating function, and an initial REPL.

Planned next steps include coefficient commands, user-defined recurrences, and generating functions defined by combinatorial equations.

## Author

Hayk Minasyan

Individual Software Project, Charles University

Supervisor: Vít Šefl
