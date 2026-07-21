# GFComb
 
GFComb is a Haskell library and command-line tool for working with ordinary generating functions.
 
The project currently supports:
 
* formal power series over rational numbers, including formal square roots;
* polynomial arithmetic, including root-finding via the rational root theorem;
* rational generating functions;
* linear recurrences, including exact closed-form solutions (via partial fractions, covering irrational characteristic roots such as Fibonacci's golden ratio);
* combinatorial/algebraic generating functions defined by functional equations (e.g. Catalan numbers, `C = 1 + x*C^2`), solved for coefficients of any degree via guarded self-reference;
* explicit n-th coefficients of such equations via Lagrange inversion, for equations of the form `Y = c + x*phi(Y)`;
* closed-form solutions for equations quadratic in the unknown, via the quadratic formula;
* predefined generating functions such as Fibonacci;
* a basic interactive REPL.
## Build
 
From the project root:
 
```
cabal build all
```
 
## Run tests
 
```
cabal test
```
 
## Run the REPL
 
```
cabal run gfcomb
```
 
## Available REPL commands
 
```
help
list
show NAME
quit
exit
```
 
Example:
 
```
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
 
```
app/REPL.hs                 Interactive command-line interface
src/GFComb/Core.hs          Formal power series
src/GFComb/Polynomial.hs    Polynomial operations
src/GFComb/RationalGF.hs    Rational generating functions
src/GFComb/Recurrence.hs    Linear recurrences and their closed-form solutions
src/GFComb/AlgebraicGF.hs   Combinatorial/algebraic generating functions
src/GFComb/Builtins.hs      Predefined generating functions
test/Main.hs                Tests
```
 
## Current status
 
The current implementation provides the full formal power series and polynomial toolkit, exact closed-form solutions for linear recurrences, combinatorial/algebraic generating functions (coefficients of any degree, Lagrange inversion, and quadratic closed forms), Fibonacci as a predefined generating function, and an initial REPL.
 
Planned next steps: more predefined generating functions (Catalan numbers, binary/ternary trees, integer partitions), and a REPL that can define, solve, and query these generating functions interactively.
 
## Author
 
Hayk Minasyan
Individual Software Project, Charles University
Supervisor: Vít Šefl