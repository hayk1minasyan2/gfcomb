module GFComb.AlgebraicGF
  ( -- Equation representation
    Expr (..),

    -- Solving: coefficients via guarded self-reference
    solveEquation
  )
where

import GFComb.Core (GF, gfAdd, gfConstant, gfMul, gfPow, gfShift, gfSub, gfVariable)

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

