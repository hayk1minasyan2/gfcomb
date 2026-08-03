-- | Combinatorial\/algebraic generating functions defined by a functional
-- equation Y = rhs: solving for coefficients of any degree via guarded
-- self-reference, explicit n-th coefficients via Lagrange inversion, and
-- a closed form for equations quadratic in the unknown.
module GFComb.AlgebraicGF
  ( -- * Equation representation
    Expr (..),

    -- * Solving: coefficients via guarded self-reference
    solveEquation,

    -- * Lagrange inversion for Y = c + x*phi(Y)
    asLagrangeForm,
    lagrangeCoefficient,
    lagrangeCoefficients,

    -- * Closed form for equations quadratic in the unknown
    QuadraticInY (..),
    asQuadraticInY,
    algebraicClosedForm
  )
where

import GFComb.Core
  ( GF,
    gfAdd,
    gfCoefficients,
    gfConstant,
    gfConstantTerm,
    gfDivide,
    gfFromCoefficients,
    gfMul,
    gfPow,
    gfScale,
    gfShift,
    gfSqrtWithSeed,
    gfSub,
    gfVariable
  )

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
import Numeric.Natural (Natural)

-------------------------------------
-- Equations -> Y = RightHandSide, for an unknown generating function Y
------------------------------------
-- | An 'Expr' is the right-hand side of a combinatorial specification such
-- as Y = 1 + x*Y^2 (Catalan numbers) or Y = x*(1 + Y^2 + Y^3) (ternary
-- trees). 'X' stands for the generating-function variable and 'Y' stands
-- for the unknown being solved for; there is no separate representation
-- of the left-hand side \"Y =\", since every equation in this module is
-- implicitly of that form.
data Expr = X | 
            Y |
            Lit Rational | 
            Add Expr Expr | 
            Sub Expr Expr | 
            Mul Expr Expr | 
            Pow Expr Natural
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
evalExpr (Pow a n) y = gfPow (evalExpr a y) n

-- Recognise an expression that is exactly a power of x (x itself, or
-- x^n), with no other content, so that multiplying by it can become a
-- 'gfShift' instead of a 'gfMul'.
xPower :: Expr -> Maybe Int
xPower X = Just 1
xPower (Pow X n) = Just (fromIntegral n)
xPower _ = Nothing

------------------------------------------------------
-- Solving Y = R.H.S. for coefficients, via guarded self-reference
------------------------------------------------------

-- | Solve an equation Y = rhs for Y as a formal power series.
--
-- This works for any equation of the \"guarded\" form Y = phi(x, Y) where
-- every occurrence of Y on the right is reached through at least one
-- multiplication by x. This covers equations of any degree in Y: 
-- Catalan numbers (Y = 1 + x*Y^2), ternary trees
-- (Y = x*(1 + Y^2 + Y^3)), and so on.
--
-- An equation that is not guarded this way (for example Y = Y + 1, where
-- Y appears with no x anywhere) will not terminate: producing 'rhs'
-- from an already-guarded combinatorial specification is the caller's
-- responsibility.
--
-- >>> take 5 (gfCoefficients (solveEquation (Add (Lit 1) (Mul X (Pow Y 2)))))
-- [1 % 1,1 % 1,2 % 1,5 % 1,14 % 1]
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

-- | Recognise an equation Y = R.H.S of the shape Y = c + x*phi(Y), where c is
-- a rational constant and phi is a polynomial expression in Y alone (no
-- x inside phi) : the shape Lagrange inversion applies to. c may be 0.
--
-- This works by fully expanding 'R.H.S.' into a flat sum of signed
-- multiplicative terms: distributing every multiplication over every
-- addition\/subtraction it touches, so e.g. x*(1 + Y^2 + Y^3) is treated
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
--
-- >>> fmap fst (asLagrangeForm (Add (Lit 1) (Mul X (Pow Y 2))))
-- Just (1 % 1)
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
xDegreeAndRemainder (Pow X n) = Just (fromIntegral n, Lit 1)
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
evalConstExpr (Pow a n) = (^ n) <$> evalConstExpr a

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
evalExprAsPolynomial (Pow a n) argument = polynomialPow (evalExprAsPolynomial a argument) n

-- | The n-th coefficient of the solution to Y = c + x*phi(Y), via the
-- Lagrange inversion formula.
--
-- Writing D = Y - c, D satisfies D = x*psi(D) with psi(z) = phi(z + c),
-- and Lagrange inversion gives
--
-- > d_0 = 0,   d_n = (1/n) * [x^(n-1)] psi(x)^n   for n >= 1
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

-- | The first 'count' coefficients (n = 0, 1, ..., count - 1) of the
-- solution to Y = c + x*phi(Y), via 'lagrangeCoefficient'.
--
-- >>> lagrangeCoefficients 1 (Pow Y 2) 5
-- [1 % 1,1 % 1,2 % 1,5 % 1,14 % 1]
lagrangeCoefficients :: Rational -> Expr -> Int -> [Rational]
lagrangeCoefficients c phi count = [lagrangeCoefficient c phi n | n <- [0 .. count - 1]]


------------------------------------
-- Closed form for equations quadratic in the unknown
-------------------------------------
 
-- | The three coefficient expressions of a(x)*Y^2 + b(x)*Y + c(x) = 0,
-- obtained by rearranging Y = R.H.S. None of quadA, quadB, quadC contain
-- 'Y' (they are the \"coefficients\", in x alone)
data QuadraticInY = QuadraticInY
  { quadA :: Expr,
    -- ^ The coefficient of Y^2, i.e. a(x).
    quadB :: Expr,
    -- ^ The coefficient of Y, i.e. b(x).
    quadC :: Expr
    -- ^ The constant term, i.e. c(x).
  }
  deriving (Eq, Show)
 
-- Does an expression contain 'Y' anywhere?
containsY :: Expr -> Bool
containsY X = False
containsY Y = True
containsY (Lit _) = False
containsY (Add a b) = containsY a || containsY b
containsY (Sub a b) = containsY a || containsY b
containsY (Mul a b) = containsY a || containsY b
containsY (Pow a _) = containsY a
 
-- | Recognise an equation Y = rhs as being quadratic in Y, i.e. rhs - Y can
-- be written as a(x)*Y^2 + b(x)*Y + c(x) for some x-only expressions
-- a, b, c.
--
-- Like 'asLagrangeForm', this works by fully expanding rhs into signed
-- multiplicative terms (distributing Mul over Add\/Sub via
-- 'expandedTerms') and classifying each term by its degree in Y (0, 1,
-- or 2); a degree outside {0, 1, 2} means the equation isn't quadratic in
-- Y and this returns 'Nothing' (this is out of the scope of the project.
-- A quadratic equation always has a closed radical solution (the quadratic formula)
-- Cubics and quartics also technically have closed forms (Cardano's and Ferrari's formulas), 
-- but they're substantially messier. Quintics and beyond generally have no closed radical form at all). 
-- The degree-1 bucket has 1 subtracted ('Sub _ (Lit 1)') to account for moving Y from the equation's left
-- side to the right side.
--
-- An equation with no Y^2 term at all (e.g. a plain linear equation) is
-- not what this function is for, and also returns 'Nothing'.
-- 'solveEquation' handles those directly.
asQuadraticInY :: Expr -> Maybe QuadraticInY
asQuadraticInY rhs = do
  classifiedTerms <- mapM classifyTerm (expandedTerms rhs)
  let byDegree degree = [term | (d, term) <- classifiedTerms, d == degree]
      sumTerms terms = case terms of
        [] -> Lit 0
        (firstTerm : remainingTerms) -> foldl Add firstTerm remainingTerms
  case byDegree 2 of
    [] -> Nothing
    quadraticTerms ->
      Just
        ( QuadraticInY
            (sumTerms quadraticTerms)
            (Sub (sumTerms (byDegree 1)) (Lit 1))
            (sumTerms (byDegree 0))
        )
  where
    classifyTerm (isPositive, term) = do
      (degree, remainder) <- yDegreeAndRemainder term
      if degree < 0 || degree > 2
        then Nothing
        else Just (degree, if isPositive then remainder else Sub (Lit 0) remainder)
 
-- The Y-degree analogue of 'xDegreeAndRemainder'. For a single
-- multiplicative term (no Add/Sub inside), count how many factors of Y it
-- has and return what's left after removing them all.
yDegreeAndRemainder :: Expr -> Maybe (Int, Expr)
yDegreeAndRemainder X = Just (0, X)
yDegreeAndRemainder Y = Just (1, Lit 1)
yDegreeAndRemainder (Lit c) = Just (0, Lit c)
yDegreeAndRemainder (Pow Y n) = Just (fromIntegral n, Lit 1)
yDegreeAndRemainder (Pow base n)
  | containsY base = Nothing
  | otherwise = Just (0, Pow base n)
yDegreeAndRemainder (Mul a b) = do
  (degreeA, remainderA) <- yDegreeAndRemainder a
  (degreeB, remainderB) <- yDegreeAndRemainder b
  Just (degreeA + degreeB, Mul remainderA remainderB)
yDegreeAndRemainder (Add _ _) = Nothing -- this is already handled by 'expandedTerms', and shouldn't occurre
yDegreeAndRemainder (Sub _ _) = Nothing -- this is already handled by 'expandedTerms', and shouldn't occurre
 
-- | Solve a(x)*Y^2 + b(x)*Y + c(x) = 0 for Y as a formal power series, via
-- the quadratic formula
--
-- > Y = ( -b(x) +- sqrt(b(x)^2 - 4*a(x)*c(x)) ) / (2*a(x))
--
-- given the caller's expected value of Y(0) (e.g. 1 for Catalan
-- numbers, since C(0) = 1). This single expected value is enough to
-- determine everything else:
--
--   * It must satisfy the equation evaluated at x = 0,
--     a(0)*y0^2 + b(0)*y0 + c(0) = 0 - checked explicitly up front, so a
--     wrong Y(0) is reported clearly rather than producing a wrong
--     series. (This check matters even when a(0) = 0, the most common
--     case for combinatorial specifications, where the +-sqrt branch
--     choice below turns out not to depend on Y(0) at all, and only this
--     explicit check catches a wrong Y(0) in that case.)
--   * Rearranging the quadratic formula at x = 0 gives the exact seed
--     'gfSqrtWithSeed' needs for the discriminant's square root.
--     seed = 2*a(0)*y0 + b(0). This is always a valid seed when y0
--     satisfies the equation above (squaring it reproduces the
--     discriminant's constant term exactly), so the caller never has to
--     separately guess which of the two square roots to use.
--
-- When a(x) has a zero constant term, the division by 2*a(x) has a removable
-- singularity at x = 0 - both the numerator and denominator vanish together, 
-- and the shared factor of x is taken out from each before dividing 
-- so that the result is still a valid formal power series.
--
-- >>> fmap (take 5 . gfCoefficients) (algebraicClosedForm (Add (Lit 1) (Mul X (Pow Y 2))) 1)
-- Right [1 % 1,1 % 1,2 % 1,5 % 1,14 % 1]
algebraicClosedForm :: Expr -> Rational -> Either String GF
algebraicClosedForm rhs expectedConstantTerm =
  case asQuadraticInY rhs of
    Nothing -> Left "this equation is not quadratic in the unknown"
    Just (QuadraticInY aExpr bExpr cExpr)
      | a0 * y0 * y0 + b0 * y0 + c0 /= 0 ->
          Left
            ( show expectedConstantTerm
                ++ " does not satisfy the equation at x = 0 (with a(0) = "
                ++ show a0
                ++ ", b(0) = "
                ++ show b0
                ++ ", c(0) = "
                ++ show c0
                ++ ")"
            )
      | otherwise ->
          case gfSqrtWithSeed seed discriminant of
            Left err -> Left ("could not take the square root of the discriminant: " ++ show err)
            Right sqrtDiscriminant -> divideRemovingCommonZero (gfSub sqrtDiscriminant bGF) (gfScale 2 aGF)
      where
        aGF = evalExpr aExpr Nothing
        bGF = evalExpr bExpr Nothing
        cGF = evalExpr cExpr Nothing
        a0 = gfConstantTerm aGF
        b0 = gfConstantTerm bGF
        c0 = gfConstantTerm cGF
        y0 = expectedConstantTerm
        seed = 2 * a0 * y0 + b0
        discriminant = gfSub (gfMul bGF bGF) (gfScale 4 (gfMul aGF cGF))
 
-- Divide two series, stripping a shared factor of x from both first if
-- the denominator's constant term is zero but the numerator's constant
-- term is zero too.

divideRemovingCommonZero :: GF -> GF -> Either String GF
divideRemovingCommonZero = go (0 :: Int)
  where
    maxStrips = 10 :: Int
    -- If we've already stripped a factor of x maxStrips times 
    -- and denominator still has a zero constant term, give up 
    -- with a clear message rather than looping forever 
    -- (this only matters for a improperly formatted equation.
    -- every well-formed one we'll actually feed in it needs at most one or two strips).
    
    go strips numerator denominator
      | gfConstantTerm denominator /= 0 =
          case gfDivide numerator denominator of
            Left err -> Left ("division failed: " ++ show err)
            Right result -> Right result
      | strips >= maxStrips =
          Left "the denominator's constant term is still zero after removing several common factors of x"
      | gfConstantTerm numerator /= 0 =
          Left "the numerator and denominator do not share a removable factor of x (division by a series with a zero constant term)"
      | otherwise = go (strips + 1) (gfDivideByX numerator) (gfDivideByX denominator)
 
-- Divide a series by x once, dropping its (necessarily zero) constant
-- term. Only meaningful when that constant term actually is 0. used only
-- by 'divideRemovingCommonZero', which checks that itself.
gfDivideByX :: GF -> GF
gfDivideByX gf = gfFromCoefficients (tail (gfCoefficients gf))