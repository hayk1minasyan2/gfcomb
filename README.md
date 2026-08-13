# GFComb

A Haskell library and REPL for combinatorial generating functions.

- Formal power series over the rationals, with exact arithmetic throughout — no floating point anywhere.
- Linear recurrences: their generating functions, their terms, and exact closed forms via a `p + q*sqrt(d)` extension of the rationals. Homogeneous, or with a polynomial forcing term.
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
 
The test suite is built on `tasty`, combining example-based unit tests (`tasty-hunit`) with property-based tests (`tasty-quickcheck`) that check the algebraic laws the library should satisfy — commutativity, associativity, identities, closed forms against directly computed recurrence terms, and that printing an equation and parsing it back returns the original.
 
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

Then, for a tour of everything below:
 
```
> load examples.gfcomb
```
 
Command history is kept in `.gfcomb_history` in the working directory, so it survives between sessions. Ctrl-D leaves; Ctrl-C abandons a long-running command without ending the session.
 
## REPL commands

### Defining by a recurrence

The name of the sequence on the left is yours to choose, and every mention must match it:
 
```
> define fib by recurrence: a(n) = a(n-1) + a(n-2), a(0)=1, a(1)=1
Generating function closed form: 1 / (1 - x - x^2)
Closed form: a(n) = (1/2 + 1/10*sqrt(5)) * (1/2 + 1/2*sqrt(5))^n + (1/2 - 1/10*sqrt(5)) * (1/2 - 1/2*sqrt(5))^n
First 10 coefficients: [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

Coefficients may be rational and may be negative (`3*a(n-1) - 2*a(n-2)`). Gaps are allowed: `a(n) = a(n-1) + a(n-3)` has order 3 with a zero coefficient for `a(n-2)`. Every initial value `a(0)` through `a(k-1)` must be given, where `k` is the largest offset referred to.

**Forcing terms.** The right-hand side may also contain any polynomial in `n`:

```
> define hanoi by recurrence: a(n) = 2*a(n-1) + 1, a(0)=1
Generating function closed form: 1 / (1 - 3x + 2x^2)
Closed form: a(n) = (-1) + (2) * (2)^n
First 10 coefficients: [1, 3, 7, 15, 31, 63, 127, 255, 511, 1023]
```

Such a recurrence is not solved by a separate method: a polynomial forcing term of degree `d` is annihilated by `d+1` differences, so the recurrence is *equivalent* to a homogeneous one of order `k+d+1`, and that is what gets built. This is why `a(n) = 2*a(n-1) + n` comes out as `2^(n+1) - n - 2` — the extra root at 1 is what contributes the polynomial part.

The forcing term does not change how many initial values are needed; the extra ones the converted recurrence requires are computed rather than asked for.

**Formulas.** With no reference to an earlier term at all, the right-hand side is simply a formula, and no initial values are given because the formula already fixes every value:

```
> define squares by recurrence: a(n) = n^2
Generating function closed form: (x + x^2) / (1 - 3x + 3x^2 - x^3)
Closed form: a(n) = (1) * n^2
First 10 coefficients: [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
```

**When a closed form exists.** One is produced whenever the characteristic polynomial factors, over the rationals, into linear factors and at most one irreducible quadratic factor. Repeated rational roots are fine — they contribute `n^j` terms, as in `(1 + n) * 2^n`. Complex roots, a repeated *irrational* root, and irreducible factors of degree three or more are reported as unavailable rather than approximated. The generating function and the terms are always available regardless.

### Defining by a functional equation

The name being defined appears on both sides:

```
> define C as solution of: C = 1 + x*C^2
Defined by: C = 1 + x*C^2
Generating function closed form: (1 - sqrt(1 - 4x)) / (2x)
Lagrange form: C = 1 + x*phi, with phi = C^2
First 10 coefficients: [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
```

An equation of the shape `Y = c + x*phi(Y)` is reported as such, because
Lagrange inversion applies to it: the n-th coefficient can be had on its
own, without computing any of the earlier ones. That is a genuinely
different route to the same numbers from the guarded self-reference used
to list them.

Every occurrence of the name on the right must be multiplied by `x`, so that each coefficient depends only on earlier ones. `C = 1 + x*C^2` is fine; `C = 1 + C^2` is refused, since solving it would require knowing `C`'s first coefficient before computing it.

Equations of any degree give coefficients. A symbolic generating function is only produced when the equation is quadratic in the unknown — a cubic such as `T = 1 + x*T^3` still yields every coefficient, but no closed form:

```
> define T as solution of: T = 1 + x*T^3
Defined by: T = 1 + x*T^3
Generating function closed form: not available -- the equation is not quadratic in the unknown, so it has no closed form of this shape (coefficients can still be computed)
Lagrange form: T = 1 + x*phi, with phi = T^3
First 10 coefficients: [1, 1, 3, 12, 55, 273, 1428, 7752, 43263, 246675]
```

### Defining by an explicit formula
 
A name can also be given a generating function outright, using the same
expression language as `coeffs`:
 
```
> define pay = 1/((1 - x)*(1 - x^2)*(1 - x^5))
Generating function closed form: 1/((1 - x)*(1 - x^2)*(1 - x^5))
First 10 coefficients: [1, 1, 2, 2, 3, 4, 5, 6, 7, 8]
```
 
The expression may name anything already defined, so a specification can
be built up a piece at a time rather than written out in one go.
 
It may also refer to **the name being defined**, provided it does so
linearly. That is the sequence construction, and it needs no fixed point:
`S = A + B*S` rearranges to `S*(1 - B) = A`, so
 
```
> define sums = x^2/(1 - x)
Generating function closed form: x^2/(1 - x)
First 10 coefficients: [0, 0, 1, 1, 1, 1, 1, 1, 1, 1]
 
> define S = 1 + sums*S
Defined by: S = 1 + sums*S
Generating function closed form: 1/(1 - sums)
First 10 coefficients: [1, 0, 1, 1, 2, 3, 5, 8, 13, 21]
```
 
which is the class of ordered sums of integers greater than one, written
the way a combinatorial specification usually is.
 
A formula that refers to itself non-linearly — `S = 1 + x*S^2`, say — is
refused, with a pointer to `as solution of:`, which solves that shape by
guarded self-reference instead. The two forms divide the work: `=` handles
equations whose coefficients are themselves series but which are linear in
the unknown, and `as solution of:` handles equations of any degree in the
unknown but whose coefficients are polynomials in `x`.
 

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

`fibonacci`, `catalan`, `binaryTrees`, `ternaryTrees` and `partitions` are defined from the start:

```
> coeffs partitions 10
[1, 1, 2, 3, 5, 7, 11, 15, 22, 30]
```

## What is and is not supported

Everything below is reported clearly rather than silently approximated or wrongly answered.

Supported:

- linear recurrences of any order with rational coefficients, homogeneous or with a polynomial forcing term of any degree, and sequences given directly by a polynomial in `n`
- their generating functions and terms, always
- their closed forms, when the characteristic polynomial factors into rational roots (repeated allowed) plus at most one irreducible quadratic factor
- guarded functional equations `Y = phi(x, Y)` of any degree in `Y`, for coefficients
- Lagrange inversion for equations of the form `Y = c + x*phi(Y)`
- generating functions named directly by a formula, including
  specifications that refer to themselves linearly, as in the sequence
  construction `S = 1 + A*S`
- symbolic closed forms for equations quadratic in the unknown

Not supported:
 
- complex characteristic roots, repeated irrational roots, or irreducible factors of degree three or more
- non-polynomial forcing terms such as `2^n` or `n!`
- closed forms for cubic and higher algebraic equations (Cardano's and Ferrari's formulas are out of scope; the quintic has no general radical form at all)
- multivariate or exponential generating functions, and nonlinear or variable-coefficient recurrences
- specifications that refer to themselves non-linearly while also using
  another named series as a coefficient, such as `S = 1 + A*S^2` for a
  series `A` — the `as solution of:` form covers this shape only when the
  coefficients are polynomials in `x`
One deliberate asymmetry: the REPL prints closed forms containing `sqrt(...)`, which the query language cannot read back, since it has no square-root operator.
 
## Using GFComb as a library
 
The REPL is a thin layer over the library, which can be used directly:
 
```haskell
import Data.List.NonEmpty ((:|))
import GFComb.Core
import GFComb.Polynomial
import GFComb.Recurrence

main :: IO ()
main = do
  -- Direct power-series arithmetic: (1 + x)^2
  let x = gfVariable
  print (gfTake 5 ((gfOne + x) * (gfOne + x)))

  -- A generating function from a homogeneous recurrence
  let fibonacciRecurrence = linearRecurrence ((1, 1) :| [(1, 1)])
  print (recurrenceTerms 10 fibonacciRecurrence)
  putStrLn (showClosedForm (recurrenceClosedForm fibonacciRecurrence))

  -- The Tower of Hanoi: a(n) = 2*a(n-1) + 1, a(0) = 1
  let hanoi = forcedRecurrence ((2, 1) :| []) (polynomialFromList [1])
  print (recurrenceTerms 10 hanoi)
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
examples.gfcomb              A tour of the REPL, for use with load
gfcomb.cabal
```

The REPL's parser and evaluator sit in the library rather than beside `app/REPL.hs`, so that the test suite can reach them; `app/REPL.hs` holds only what genuinely needs `IO`.

## Author

Hayk Minasyan 

Individual Software Project, Charles University 

Supervisor: Vít Šefl