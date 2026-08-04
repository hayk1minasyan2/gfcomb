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
* predefined generating functions: Fibonacci numbers, Catalan numbers, binary and ternary trees, and integer partitions;
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

The test suite is built on `tasty`, combining example-based unit tests (`tasty-hunit`) with property-based tests (`tasty-quickcheck`) that check the algebraic laws the library should satisfy — commutativity, associativity, identities, and closed-form solutions against directly computed recurrence terms.

## Run the documentation examples

The `>>>` examples in the Haddock comments are executable and checked with `doctest`:

```
cabal install doctest --overwrite-policy=always
cabal repl --with-ghc=doctest
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
  catalan
  binaryTrees
  ternaryTrees
  partitions

gfcomb> show fibonacci
Name: fibonacci
Description: Fibonacci numbers: 1, 1, 2, 3, 5, 8, ...
Generating function: 1 / (1 - x - x^2)
First 10 coefficients: [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

gfcomb> show catalan
Name: catalan
Description: Catalan numbers: 1, 1, 2, 5, 14, 42, ...
Generating function: (1 - sqrt(1 - 4x)) / (2x)
First 10 coefficients: [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
```

## Using GFComb as a library

GFComb is a library first, with the REPL built on top of it — everything the REPL can do is also available as plain Haskell functions.

```haskell
import Data.List.NonEmpty ((:|))
import Data.Ratio (numerator)
import GFComb.Core (gfAdd, gfMul, gfOne, gfTake, gfVariable)
import GFComb.Recurrence (linearRecurrence, recurrenceTerms)

main :: IO ()
main = do
  -- Work with formal power series directly: (1+x)^2 = 1 + 2x + x^2
  let x = gfVariable
  print (map numerator (gfTake 5 (gfMul (gfAdd gfOne x) (gfAdd gfOne x))))
  -- [1,2,1,0,0]

  -- Or build a generating function from a linear recurrence
  let fibonacciRecurrence = linearRecurrence ((1, 1) :| [(1, 1)])
  print (map numerator (recurrenceTerms 10 fibonacciRecurrence))
  -- [1,1,2,3,5,8,13,21,34,55]
```

(`numerator` is used above purely for cleaner console output — every coefficient here happens to be a whole number, so `numerator` recovers it as a plain `Integer`; the underlying values are exact `Rational`s throughout.)

Some other useful entry points:

* `GFComb.Polynomial` — finite polynomial arithmetic and rational-root finding, independent of the infinite-series machinery in `Core`.
* `GFComb.AlgebraicGF` — solve a combinatorial specification given as an `Expr` (e.g. `Add (Lit 1) (Mul X (Pow Y 2))` for `C = 1 + x*C^2`) via `solveEquation`, `lagrangeCoefficients`, or `algebraicClosedForm`.
* `GFComb.Builtins` — the predefined generating functions (`fibonacci`, `catalan`, `binaryTrees`, `ternaryTrees`, `partitions`) as ready-made `BuiltinGF` values.

## Project structure

```
app/REPL.hs                 Interactive command-line interface
src/GFComb/Core.hs          Formal power series
src/GFComb/Polynomial.hs    Polynomial operations
src/GFComb/RationalGF.hs    Rational generating functions
src/GFComb/Recurrence.hs    Linear recurrences and their closed-form solutions
src/GFComb/AlgebraicGF.hs   Combinatorial/algebraic generating functions
src/GFComb/Builtins.hs      Predefined generating functions
test/Main.hs                Unit and property-based tests
```

## Current status

The current implementation provides the full formal power series and polynomial toolkit, exact closed-form solutions for linear recurrences, combinatorial/algebraic generating functions (coefficients of any degree, Lagrange inversion, and quadratic closed forms), five predefined generating functions, and an initial REPL.

Planned next step: a REPL that can define, solve, and query generating functions interactively (`define`/`coeffs`/`coeff`/`add`/`load` commands, plus a small parser for equations such as `T = 1 + x*T^2`).

## Author

Hayk Minasyan
Individual Software Project, Charles University
Supervisor: Vít Šefl