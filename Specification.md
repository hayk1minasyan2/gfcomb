# Specification: Individual Software Porject
# A Haskell Library and REPL for Combinatorial Generating Functions: GFComb
 
**Author:** Hayk Minasyan 

**Supervisor:** Vít Šefl

**Language:** Haskell  

---
 
## 1. Motivation and Goals
 
Generating functions are one of the most powerful tools in combinatorial counting. Given a combinatorial sequence, such as the number of binary trees with n vertices, the number of integer partitions of n, or the number of well-matched bracket strings of length 2n, a generating function encodes the entire sequence as a single algebraic object. This allows one to apply tools from algebra and analysis to answer combinatorial questions: find closed-form formulas, derive recurrence relations, and estimate growth rates.
 
Haskell is a uniquely natural language for this domain. Its lazy evaluation model allows infinite sequences to be represented and manipulated directly, without any special tricks. A generating function is, formally, a formal power series (an infinite sequence of coefficients) and in Haskell this is simply a lazy list. Arithmetic on formal power series maps cleanly onto pure functional operations.
 
The goal of this project is to implement **GFComb**: a Haskell library and interactive REPL (Read-Eval-Print Loop) for working with ordinary generating functions in the context of combinatorial counting. The project has two tightly coupled components:
 
- **A core library** implementing formal power series as a first-class Haskell type, with a full set of algebraic operations and combinatorial utilities.
- **An interactive REPL** that allows the user to define sequences and generating functions, perform computations, and extract information without writing any Haskell code themselves.
 
---

## 2. Mathematical Background
 
This section briefly defines the mathematical objects the project works with.
 
### 2.1 Formal Power Series
 
A **formal power series** over the rationals is an expression of the form:
 
```
A(x) = a_0 + a_1*x + a_2*x^2 + a_3*x^3 + ...
```
 
where the coefficients `a_0, a_1, a_2, ...` are rational numbers. Unlike an analytic function, a formal power series is not required to converge - it is treated purely algebraically. The set of formal power series over Q forms a ring under addition and multiplication (convolution):
 
- **Addition:** `(A + B)[n] = a_n + b_n`
- **Multiplication (convolution):** `(A * B)[n] = sum_{k=0}^{n} a_k * b_{n-k}`
- **Derivative:** `A'[n] = (n+1) * a_{n+1}`
- **Division:** defined when the constant term of the divisor is non-zero
- **Composition:** `A(B(x))` defined when `b_0 = 0`
 
### 2.2 Generating Functions in Combinatorics
 
The **ordinary generating function (OGF)** of a sequence `(a_n)` is the formal power series `A(x) = sum a_n * x^n`. In combinatorics, `a_n` typically counts the number of objects of "size n" in some combinatorial class. Key examples (all of which the project will support) include:
 
- **Fibonacci / aa-avoiding strings:** `A(x) = (1 + x) / (1 - x - x^2)`
- **Catalan numbers:** `C(x) = (1 - sqrt(1 - 4x)) / (2x)`
- **Integer partitions:** `P(x) = product_{k>=1} 1/(1 - x^k)`
- **Binary/ternary trees:** defined by polynomial functional equations, e.g. `T(x) = 1 + x*T(x)^2`
 
### 2.3 Key Techniques
 
The project implements and exposes the following techniques:
 
- **Solving linear recurrences:** Given initial conditions and a linear recurrence `a_n = c_1*a_{n-1} + ... + c_k*a_{n-k}`, derive the rational OGF.
- **Partial fraction decomposition:** Decompose a rational GF into simpler terms to extract a closed-form formula for `a_n`.
- **Generalized binomial formula:** Handle algebraic GFs of the form `(1 + x)^r` for rational `r`, enabling, for example, the computation of Catalan numbers from `sqrt(1 - 4x)`.
- **Coefficient extraction:** Compute the n-th coefficient of a GF efficiently.
- **Product formulas:** Represent GFs defined as infinite products (e.g. partition generating functions).
 
---
 
## 3. Planned Features
 
### 3.1 Core Library
 
The central data type is:
 
```haskell
newtype GF = GF [Rational]
```
 
representing a formal power series as a lazy (potentially infinite) list of rational coefficients. The following operations will be implemented:
 
- `gfAdd  :: GF -> GF -> GF`  - termwise addition
- `gfSub  :: GF -> GF -> GF`  - termwise subtraction
- `gfMul  :: GF -> GF -> GF`  - Cauchy product (convolution)
- `gfDiv  :: GF -> GF -> GF`  - formal division (requires constant term of divisor to be nonzero)
- `gfDeriv :: GF -> GF`       - formal derivative
- `gfInteg :: GF -> GF`       - formal integral (with constant term 0)
- `gfCompose :: GF -> GF -> GF` - composition A(B(x)), requires b_0 = 0
- `gfCoeff :: GF -> Int -> Rational` - extract the n-th coefficient
- `gfTake  :: Int -> GF -> [Rational]` - take first n coefficients
 
The `GF` type will be an instance of `Num`, so that standard Haskell arithmetic syntax works naturally on generating functions.

(TO BE ADDED)

### 3.2 Recurrence Solver
 
Given a linear recurrence with constant coefficients:
 
```
a_n = c_1 * a_{n-1} + c_2 * a_{n-2} + ... + c_k * a_{n-k}
```
 
and initial conditions `a_0, ..., a_{k-1}`, this module will:
 
- Compute the generating function as a ratio of two polynomials (an explicit formula for the generating function).
- Perform partial fraction decomposition over the rationals (and, where needed, over algebraic extensions).
- Return an explicit closed-form formula for `a_n` as a linear combination of terms of the form `r^n`, `n*r^n`, etc.
 
Example: Given `a_n = a_{n-1} + a_{n-2}`, `a_0 = 1`, `a_1 = 2`, the module produces `A(x) = (1 + x) / (1 - x - x^2)` and the closed-form formula involving powers of the golden ratio.
 
### 3.3 Algebraic GF Tools
 
This module handles generating functions defined by polynomial equations, such as `C(x) = 1 + x * C(x)^2` (Catalan numbers) or `T(x) = 1 + x * T(x)^3` (ternary trees). Features:
 
- Solve simple algebraic equations for the GF using Newton's method for formal power series (iterative lifting), computing as many coefficients as needed.
- Apply the **generalized binomial formula**: for rational `r`, compute `(1 + x)^r` as a formal power series using the generalized binomial coefficients.
- Support GFs involving square roots (e.g. `sqrt(1 - 4x)`) arising from quadratic equations.
 
### 3.4 Combinatorial Library
 
A library of named, pre-defined generating functions for well-known combinatorial sequences:
 
| Name | Sequence | Generating Function |
|------|----------|---------------------|
| `fibonacci` | 1, 1, 2, 3, 5, ... | `1 / (1 - x - x^2)` |
| `catalan` | 1, 1, 2, 5, 14, ... | `(1 - sqrt(1-4x)) / (2x)` |
| `partitions` | 1, 1, 2, 3, 5, 7, ... | `prod_{k>=1} 1/(1-x^k)` |
| `binaryTrees` | 1, 1, 2, 5, 14, ... | `(1 - sqrt(1-4x)) / (2x)` |
| `ternaryTrees` | solution of `T = 1 + xT^3` | computed iteratively |
etc...
 
Each entry will include a brief combinatorial description, so the user can understand what is being counted.
 
### 3.5 Interactive REPL
 
An interactive command-line interface allowing the user to work with generating functions without writing Haskell. Commands will include:
 
```
> define fib by recurrence: a(n) = a(n-1) + a(n-2), a(0)=1, a(1)=1
Generating function: 1 / (1 - x - x^2)
Closed form: a(n) = (phi^(n+1) - psi^(n+1)) / sqrt(5)
 
> coeffs fib 10
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
 
> coeff fib 20
6765
 
> define C as solution of: C = 1 + x * C^2
Generating function: (1 - sqrt(1 - 4x)) / (2x)
This is the Catalan number generating function.
 
> coeffs C 8
[1, 1, 2, 5, 14, 42, 132, 429]
 
> add fib C
Generating function: (sum of fib and C)
First 8 coefficients: [2, 2, 4, 8, 19, 49, 145, 450]
 
> load partitions
Loaded built-in: integer partition generating function.
 
> coeffs partitions 12
[1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56]
 
> help
(displays all available commands)
```
 
The REPL will support:
- Defining GFs by recurrence, by algebraic equation, or by arithmetic on existing GFs
- Extracting individual coefficients and finite lists of coefficients
- Loading built-in named GFs from the combinatorial library
- Displaying the symbolic form of the GF where available
- Saving and loading sessions
 
---
 
## 4. References
 
- Dvořák, Z. (2026). *KG1 Notes* (Combinatorics 1 lecture notes). Charles University.
- Wilf, H. (2006). *Generatingfunctionology* (3rd ed.). A.K. Peters.
- Lipovača, M. *Learn You a Haskell for Great Good!*