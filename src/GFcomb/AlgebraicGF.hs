module GFComb.AlgebraicGF
  ( -- Equation representation
    Expr (..),

    -- Solving: coefficients via guarded self-reference
    solveEquation,

    -- Lagrange inversion for Y = c + x*phi(Y)
    asLagrangeForm,
    lagrangeCoefficient,
    lagrangeCoefficients
  )
where

import GFComb.Core (GF, gfAdd, gfConstant, gfMul, gfPow, gfShift, gfSub, gfVariable)
import GFComb.Polynomial
  ( Polynomial,
    polynomialAdd,
    polynomialCoefficientAt,
    polynomialFromList,
    polynomialMul,
    polynomialPow,
    polynomialSub,
    polynomialVariable
  )

-------------------------------------
-- Equations -> Y = RightHandSide, for an unknown generating function Y
------------------------------------
-- An 'Expr' is the right-hand side of a combinatorial specification such
-- as Y = 1 + x*Y^2 (Catalan numbers) or Y = x*(1 + Y^2 + Y^3) (ternary
-- trees). 'X' stands for the generating-function variable and 'Y' stands
-- for the unknown being solved for; there is no separate representation
-- of the left-hand side "Y =", since every equation in this module is
-- implicitly of that form.

data Expr = X | 
            Y |
            Lit Rational | 
            Add Expr Expr | 
            Sub Expr Expr | 
            Mul Expr Expr | 
            Pow Expr Int
  deriving (Eq, Show)

-------------------------
-- Evaluating an Expr as a GF
---------------------------

-- Evaluate an expression as a formal power series, given a value to use
-- for 'Y', or 'Nothing' if the expression is required not to contain
-- 'Y' at all (used for the coefficient sub-expressions inside
-- Lagrange-inversion equations, which by construction involve only 'Y'and never 'X' (or vice versa) see 'asLagrangeForm').
--
-- Multiplication is the one case that needs care: whenever one side of a
-- 'Mul' is syntactically a power of 'X' (recognised by 'xPower'), that
-- side is applied via 'gfShift' rather than 'gfMul'. This is important for efficiency and correctness.
evalExpr :: Expr -> Maybe GF -> GF
evalExpr X _ = gfVariable
evalExpr Y (Just y) = y
evalExpr Y Nothing = error "GFComb.AlgebraicGF.evalExpr: this expression is expected not to contain the unknown, but it does"
evalExpr (Lit c) _ = gfConstant c
evalExpr (Add a b) y = gfAdd (evalExpr a y) (evalExpr b y)
evalExpr (Sub a b) y = gfSub (evalExpr a y) (evalExpr b y)
evalExpr (Mul a b) y =
  case xPower a of
    Just k -> gfShift k (evalExpr b y)
    Nothing ->
      case xPower b of
        Just k -> gfShift k (evalExpr a y)
        Nothing -> gfMul (evalExpr a y) (evalExpr b y)
evalExpr (Pow a n) y = gfPow (evalExpr a y) (fromIntegral n)

-- Recognise an expression that is exactly a power of x (x itself, or
-- x^n), with no other content, so that multiplying by it can become a
-- 'gfShift' instead of a 'gfMul'.
xPower :: Expr -> Maybe Int
xPower X = Just 1
xPower (Pow X n) = Just n
xPower _ = Nothing

------------------------------------------------------
-- Solving Y = R.H.S. for coefficients, via guarded self-reference
------------------------------------------------------

-- Solve an equation Y = rhs for Y as a formal power series.
--
-- This works for any equation of the "guarded" form Y = phi(x, Y) where
-- every occurrence of Y on the right is reached through at least one
-- multiplication by x. This covers equations of any degree in Y: 
-- Catalan numbers (Y = 1 + x*Y^2), ternary trees
-- (Y = x*(1 + Y^2 + Y^3)), and so on.
--
-- An equation that is not guarded this way (for example Y = Y + 1, where
-- Y appears with no x anywhere) will not terminate: producing 'rhs'
-- from an already-guarded combinatorial specification is the caller's
-- responsibility.
solveEquation :: Expr -> GF
solveEquation rhs = fixedPoint
  where
    fixedPoint = evalExpr rhs (Just fixedPoint)

------------------------------------------------------
-- Lagrange inversion for Y = c + x*phi(Y)
------------------------------------------------------

-- Does an expression contain 'X' anywhere?
containsX :: Expr -> Bool
containsX X = True
containsX Y = False
containsX (Lit _) = False
containsX (Add a b) = containsX a || containsX b
containsX (Sub a b) = containsX a || containsX b
containsX (Mul a b) = containsX a || containsX b
containsX (Pow a _) = containsX a

-- Recognise an equation Y = R.H.S of the shape Y = c + x*phi(Y), where c is
-- a rational constant and phi is a polynomial expression in Y alone (no
-- x inside phi) : the shape Lagrange inversion applies to. c may be 0.
--
-- This works by fully expanding 'R.H.S.' into a flat sum of signed
-- multiplicative terms: distributing every multiplication over every
-- addition/subtraction it touches, so e.g. x*(1 + Y^2 + Y^3) is treated
-- exactly the same as x + x*Y^2 + x*Y^3, and then classifying each term
-- by how many factors of x it contains:
--
--   * no x at all: the term must be a plain number (no Y either), and
--     contributes to c;
--   * exactly one factor of x: the term, with that one x factor removed,
--     contributes an additive term to phi;
--   * anything else (two or more factors of x, or an x-free term that
--     still involves Y): the equation is not of this shape, and the
--     whole function returns 'Nothing'.
--
-- So both Y = 1 + x*Y + x*Y^2 (two separate x*(...) terms; c = 1,
-- phi = Y + Y^2) and Y = x*(1 + Y^2 + Y^3) (x factored out over a single
-- sum; c = 0, phi = 1 + Y^2 + Y^3) are recognised as the same shape.
-- 'solveEquation' still computes the coefficients of equations this
-- doesn't recognise, just without a Lagrange-inversion shortcut.

asLagrangeForm :: Expr -> Maybe (Rational, Expr)
asLagrangeForm rhs = do
  classifiedTerms <- mapM classifyTerm (expandedTerms rhs)
  let phiTerms = [term | Right term <- classifiedTerms]
      constantTerms = [value | Left value <- classifiedTerms]
  case phiTerms of
    [] -> Nothing
    (firstTerm : remainingTerms) -> Just (sum constantTerms, foldl Add firstTerm remainingTerms)
  where
    classifyTerm (isPositive, term) =
      case xDegreeAndRemainder term of
        Nothing -> Nothing
        Just (0, _) -> do
          value <- evalConstExpr term
          Just (Left (if isPositive then value else negate value))
        Just (1, remainder) ->
          Just (Right (if isPositive then remainder else Sub (Lit 0) remainder))
        Just (_, _) -> Nothing

-- Fully expand an expression into a flat list of signed, purely
-- multiplicative terms (no Add/Sub left inside any term), by
-- distributing multiplication over every addition/subtraction it
-- touches. e.g. x*(1 + Y^2 + Y^3) becomes the three terms x*1, x*Y^2,
-- x*Y^3, exactly as if it had been written that way to begin with. This
-- is what lets 'asLagrangeForm' recognise an x factored out over a whole
-- sum, not just an equation already written as several separate
-- x*(...) terms.
expandedTerms :: Expr -> [(Bool, Expr)]
expandedTerms (Add a b) = expandedTerms a ++ expandedTerms b
expandedTerms (Sub a b) = expandedTerms a ++ [(not isPositive, term) | (isPositive, term) <- expandedTerms b]
expandedTerms (Mul a b) =
  [ (positiveA == positiveB, Mul termA termB)
    | (positiveA, termA) <- expandedTerms a,
      (positiveB, termB) <- expandedTerms b
  ]
expandedTerms term = [(True, term)]

-- For a single multiplicative term (as produced by 'expandedTerms', so it
-- contains no Add/Sub), count how many factors of x it has and return
-- what's left after removing them all. e.g. for x*Y^2 this is
-- (1, Y^2), and for a term with no x at all it's (0, the term
-- unchanged). Returns 'Nothing' if x appears somewhere this can't
-- account for (currently: inside the base of a power of a non-x
-- expression, e.g. (x*Y)^2.

xDegreeAndRemainder :: Expr -> Maybe (Int, Expr)
xDegreeAndRemainder X = Just (1, Lit 1)
xDegreeAndRemainder Y = Just (0, Y)
xDegreeAndRemainder (Lit c) = Just (0, Lit c)
xDegreeAndRemainder (Pow X n) = Just (n, Lit 1)
xDegreeAndRemainder (Pow base n)
  | containsX base = Nothing
  | otherwise = Just (0, Pow base n)
xDegreeAndRemainder (Mul a b) = do
  (degreeA, remainderA) <- xDegreeAndRemainder a
  (degreeB, remainderB) <- xDegreeAndRemainder b
  Just (degreeA + degreeB, Mul remainderA remainderB)
xDegreeAndRemainder (Add _ _) = Nothing
xDegreeAndRemainder (Sub _ _) = Nothing

-- Evaluate an expression that should contain neither 'X' nor 'Y' down to
-- a plain rational number, or 'Nothing' if it contains either.
evalConstExpr :: Expr -> Maybe Rational
evalConstExpr X = Nothing
evalConstExpr Y = Nothing
evalConstExpr (Lit c) = Just c
evalConstExpr (Add a b) = (+) <$> evalConstExpr a <*> evalConstExpr b
evalConstExpr (Sub a b) = (-) <$> evalConstExpr a <*> evalConstExpr b
evalConstExpr (Mul a b) = (*) <$> evalConstExpr a <*> evalConstExpr b
evalConstExpr (Pow a n)
  | n < 0 = Nothing
  | otherwise = (^ n) <$> evalConstExpr a

-- Evaluate an expression as a finite polynomial in a single variable,
-- substituting the given polynomial for every occurrence of 'Y'. The
-- expression must not contain 'X'.
--
-- This is how 'lagrangeCoefficient' below builds psi(z) = phi(z + c): it
-- substitutes the polynomial (z + c) for phi's 'Y', evaluating the whole
-- expression tree with GFComb.Polynomial's arithmetic instead of GF's.

evalExprAsPolynomial :: Expr -> Polynomial -> Polynomial
evalExprAsPolynomial X _ = error "GFComb.AlgebraicGF.evalExprAsPolynomial: this expression is expected not to contain x"
evalExprAsPolynomial Y argument = argument
evalExprAsPolynomial (Lit c) _ = polynomialFromList [c]
evalExprAsPolynomial (Add a b) argument = polynomialAdd (evalExprAsPolynomial a argument) (evalExprAsPolynomial b argument)
evalExprAsPolynomial (Sub a b) argument = polynomialSub (evalExprAsPolynomial a argument) (evalExprAsPolynomial b argument)
evalExprAsPolynomial (Mul a b) argument = polynomialMul (evalExprAsPolynomial a argument) (evalExprAsPolynomial b argument)
evalExprAsPolynomial (Pow a n) argument = polynomialPow (evalExprAsPolynomial a argument) (fromIntegral n)

-- The n-th coefficient of the solution to Y = c + x*phi(Y), via the
-- Lagrange inversion formula.
--
-- Writing D = Y - c, D satisfies D = x*psi(D) with psi(z) = phi(z + c),
-- and Lagrange inversion gives
--
--   d_0 = 0,   d_n = (1/n) * [x^(n-1)] psi(x)^n   for n >= 1
--
-- with t_0 = c and t_n = d_n for n >= 1. psi(x)^n is a finite polynomial
-- of degree n * degree(phi), computed with the already-tested
-- 'polynomialPow' - this reads off a single coefficient of Y directly,
-- without computing any of Y's other coefficients along the way, unlike
-- 'solveEquation'.

lagrangeCoefficient :: Rational -> Expr -> Int -> Rational
lagrangeCoefficient c _ 0 = c
lagrangeCoefficient _ _ n | n < 0 = 0
lagrangeCoefficient c phi n =
  polynomialCoefficientAt (polynomialPow psi (fromIntegral n)) (n - 1) / fromIntegral n
  where
    psi = evalExprAsPolynomial phi (polynomialAdd polynomialVariable (polynomialFromList [c]))

-- The first 'count' coefficients (n = 0, 1, ..., count - 1) of the
-- solution to Y = c + x*phi(Y), via 'lagrangeCoefficient'.
lagrangeCoefficients :: Rational -> Expr -> Int -> [Rational]
lagrangeCoefficients c phi count = [lagrangeCoefficient c phi n | n <- [0 .. count - 1]]

