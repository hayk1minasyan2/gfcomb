# GFComb

A Haskell library and REPL for combinatorial generating functions.

- Formal power series over the rationals, with exact rational arithmetic throughout — no floating point anywhere.
- Linear recurrences: their generating functions, their terms, and exact closed forms via partial fractions over a `p + q*sqrt(d)` extension of the rationals.
- Combinatorial specifications given as functional equations (`C = 1 + x*C^2`), solved for coefficients by guarded self-reference, for individual coefficients by Lagrange inversion, and — when the equation is quadratic in the unknown — for a symbolic closed form.
- A REPL for defining and querying generating functions interactively.

## Build

```
cabal build all
```

## Run tests

```
cabal test
```

The test suite is built on `tasty`, combining example-based unit tests (`tasty-hunit`) with property-based tests (`tasty-quickcheck`) that check the algebraic laws the library should satisfy — commutativity, associativity, identities, closed-form solutions against directly computed recurrence terms, and that printing an equation and parsing it back returns the original.

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

Command history is kept in `.gfcomb_history` in the working directory, so it survives between sessions. Ctrl-D leaves; Ctrl-C abandons a long-running command without ending the session.

## REPL commands

### Defining

A generating function can be defined in either of two ways, matching the two solvers in the library.

**By a linear recurrence.** The name of the sequence on the left is yours to choose, and every mention must match it:

```
> define fib by recurrence: a(n) = a(n-1) + a(n-2), a(0)=1, a(1)=1
Generating function: 1 / (1 - x - x^2)
Closed form: a(n) = (1/2 + 1/10*sqrt(5)) * (1/2 + 1/2*sqrt(5))^n + (1/2 - 1/10*sqrt(5)) * (1/2 - 1/2*sqrt(5))^n
First 10 coefficients: [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

Coefficients may be rational and may be negative (`3*a(n-1) - 2*a(n-2)`). Gaps are allowed: `a(n) = a(n-1) + a(n-3)` has order 3 with a zero coefficient for `a(n-2)`. Every initial value `a(0)` through `a(k-1)` must be given, where `k` is the order.

A closed form is found when the characteristic polynomial factors into distinct roots that are rational, or rational plus one irreducible quadratic factor. Repeated roots would need an extra `n * r^n` term, and complex roots are outside what this represents; both are reported rather than silently approximated.

**By a functional equation.** The name being defined appears on both sides:

```
> define C as solution of: C = 1 + x*C^2
Defined by: C = 1 + x*C^2
Generating function: (1 - sqrt(1 - 4x)) / (2x)
First 10 coefficients: [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
```

Every occurrence of the name on the right must be multiplied by `x`, so that each coefficient depends only on earlier ones. `C = 1 + x*C^2` is fine; `C = 1 + C^2` is refused, since solving it would require knowing `C`'s first coefficient before computing it.

Equations of any degree give coefficients. A symbolic generating function is only produced when the equation is quadratic in the unknown — a cubic such as `T = 1 + x*T^3` still yields every coefficient, but no closed form:

```
> define T as solution of: T = 1 + x*T^3
Defined by: T = 1 + x*T^3
Generating function: no closed form available -- the equation is not quadratic in the unknown, so it has no closed form of this shape (coefficients can still be computed)
First 10 coefficients: [1, 1, 3, 12, 55, 273, 1428, 7752, 43263, 246675]
```

### Querying

```
> coeffs fib 10          the first 10 coefficients
> coeff fib 20           the coefficient of x^20
> add fib C              the first 10 coefficients of fib + C
> show fib               describe one definition
> list                   every defined name, and how it was defined
```

`coeffs` and `coeff` take a whole expression, not just a name. An expression combines defined names with `+`, `-`, `*`, `/` and `^`, may use rational literals, and may mention `x`:

```
> coeffs catalan + fibonacci 10
> coeffs 1/(1 - x) 5
> coeffs C^2 6
> coeff C*T 4
```

Division fails when the divisor's constant term is zero, which is the one case where formal power series division is undefined.

**Multiplication must be written explicitly**: `x*C^2`, not `xC^2`. Allowing juxtaposition would make `xT` ambiguous between one name and a product of two.

### Everything else

```
> load FILE              run each line of a file as though it were typed
> help                   the command list
> quit, exit             leave
```

A file read by `load` may contain `--` line comments, and may itself contain a `load`. Parse errors from a file are labelled with the file and line number.

### Built-ins

`fibonacci`, `catalan`, `binaryTrees`, `ternaryTrees` and `partitions` are defined from the start — no import step, they are simply already in scope:

```
> coeffs partitions 10
[1, 1, 2, 3, 5, 7, 11, 15, 22, 30]
```

## Using GFComb as a library

The REPL is a thin layer over the library, which can be used directly:

```haskell
import Data.List.NonEmpty ((:|))
import GFComb.Core
import GFComb.Recurrence

main :: IO ()
main = do
  -- Direct power-series arithmetic: (1 + x)^2
  let x = gfVariable
  print (gfTake 5 ((gfOne + x) * (gfOne + x)))

  -- A generating function from a linear recurrence
  let fibonacciRecurrence = linearRecurrence ((1, 1) :| [(1, 1)])
  print (recurrenceTerms 10 fibonacciRecurrence)
  putStrLn (showClosedForm (recurrenceClosedForm fibonacciRecurrence))
```

`GFComb.Polynomial` provides finite polynomials with exact rational coefficients and rational root finding; `GFComb.RationalGF` the quotient `P(x)/Q(x)`; `GFComb.AlgebraicGF` functional equations, Lagrange inversion, and symbolic rendering; `GFComb.Builtins` the predefined entries. The REPL's own parser and evaluator live in `GFComb.REPL.*` and are usable independently of the interactive loop.

## Project structure

```
src/GFComb/Core.hs           Formal power series over the rationals
src/GFComb/Polynomial.hs     Finite polynomials, rational root finding
src/GFComb/Conversion.hs     Polynomial to power series
src/GFComb/RationalGF.hs     Quotients P(x)/Q(x)
src/GFComb/Recurrence.hs     Linear recurrences, surds, closed forms
src/GFComb/AlgebraicGF.hs    Functional equations, Lagrange inversion
src/GFComb/Builtins.hs       Predefined generating functions
src/GFComb/REPL/Command.hs   The REPL's abstract syntax
src/GFComb/REPL/Parser.hs    megaparsec parser for commands and expressions
src/GFComb/REPL/Eval.hs      Environment and command evaluation
app/REPL.hs                  haskeline input loop
test/Main.hs                 Unit and property-based tests
gfcomb.cabal
```

The REPL's parser and evaluator sit in the library rather than beside `app/REPL.hs`, so that the test suite can reach them; `app/REPL.hs` holds only what genuinely needs `IO`.

## Author

Hayk Minasyan 

Individual Software Project, Charles University 

Supervisor: Vít Šefl