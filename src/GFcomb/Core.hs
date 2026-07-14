
module GFComb.Core
  ( -- Generating functions
    GF,

    -- Errors
    GFError (..),

    -- Construction
    gfFromList,
    gfFromCoefficients,
    gfConstant,
    gfZero,
    gfOne,
    gfVariable,

    -- Coefficient access
    gfCoefficients,
    gfConstantTerm,
    gfCoeffAt,
    gfCoeffAtMaybe,
    gfTake,

    -- Basic arithmetic
    gfAdd,
    gfSub,
    gfNegate,
    gfScale,
    gfMul,
    gfPow,

    -- Formal power-series operations
    gfDivide,
    gfReciprocal,
    gfDerivative,
    gfIntegral,
    gfCompose,

    -- Comparison
    gfEqualUpTo
  )
where


import Data.List (intercalate)
import Data.Ratio (denominator, numerator)
import Numeric.Natural (Natural)


-- A formal power series over the rationals.
-- A(x) = a_0 + a_1*x + a_2*x^2 + ... is represented as GF [a_0, a_1, a_2, ...]
-- Examples:
--   GF [1, 2, 3, 0, 0, ...] represents 1 + 2x + 3x^2
--   GF [1, 1, 1, 1, ...] represents 1/(1-x) (geometric series)

newtype GF = GF [Rational]

---------------------------------------------------------------------

-- Errors that may occur while manipulating formal power series.
data GFError = DivisionByZeroConstant | CompositionRequiresZeroConstant Rational
    deriving (Eq, Show)

--------------------------------------------------------------------

-- Displaying a GF by showing the first 10 coefficients
instance Show GF where
  show gf = "GF ["
                ++ intercalate ", " (map showCoeff (gfTake 10 gf))
                ++ ", ...]"
    where
      showCoeff coefficient
        | denominator coefficient == 1 = show (numerator coefficient)
        | otherwise = show coefficient

instance Fractional GF where
  fromRational = gfConstant

  recip gf =
    case gfReciprocal gf of
      Left err -> error ("cannot invert formal power series: " ++ show err)
      Right result -> result

-- I am making GF a Num instance so that we can use +, -, * syntax
instance Num GF where
  (+) = gfAdd
  (-) = gfSub
  (*) = gfMul
  negate = gfNegate
  fromInteger = gfConstant . fromInteger

  abs _ = error "abs is not defined for formal power series"

  signum _ = error "signum is not defined for formal power series"

------------------
-- Constructors
------------------


gfFromCoefficients :: [Rational] -> GF
gfFromCoefficients coefficients = GF (coefficients ++ repeat 0)

gfFromList :: [Rational] -> GF
gfFromList = gfFromCoefficients

-- Construct a constant formal power series.
gfConstant :: Rational -> GF
gfConstant constant = GF (constant : repeat 0)

gfZero :: GF
gfZero = gfConstant 0

gfOne :: GF
gfOne = gfConstant 1

gfVariable :: GF
gfVariable = GF (0 : 1 : repeat 0)

---------------------------
-- Coefficient access
---------------------------

gfCoefficients :: GF -> [Rational]
gfCoefficients (GF coefficients) = coefficients

gfConstantTerm :: GF -> Rational
gfConstantTerm (GF coefficients) = case coefficients of
        constant : _ -> constant
        [] ->   error "GF invariant violated: empty coefficient list"


gfCoeffAt :: GF -> Int -> Rational
gfCoeffAt gf index =
  case gfCoeffAtMaybe gf index of
    Nothing -> error ( "gfCoeffAt: coefficient index must be non-negative, but received "++ show index )
    Just coefficient -> coefficient


gfCoeffAtMaybe :: GF -> Int -> Maybe Rational
gfCoeffAtMaybe _ index
  | index < 0 = Nothing
gfCoeffAtMaybe (GF coefficients) index = Just (coefficients !! index)


gfTake :: Int -> GF -> [Rational]
gfTake count _
  | count <= 0 = []
gfTake count (GF coefficients) =
  take count coefficients
  
-- ----------
-- Basic operations on GFs
-- ----------

-- Addition: Adding two generating functions termwise
-- (A + B)[n] = a_n + b_n
gfAdd :: GF -> GF -> GF
gfAdd (GF as) (GF bs) = GF (zipWith (+) as bs)


-- Subtraction: Subtracting two generating functions termwise
-- (A - B)[n] = a_n - b_n
gfSub :: GF -> GF -> GF
gfSub (GF as) (GF bs) = GF (zipWith (-) as bs)


-- Multiplication: The coefficient of x^n in the product A * B is given by the convolution of the coefficients:
-- (A * B)[n] = sum_{i=0}^n a_i * b_{n-i}
-- Instead we can use Cauchy's product: A(x)B(x)=a0*​B(x) + x*(A_Tail​(x) * B(x)).
gfMul :: GF -> GF -> GF
gfMul (GF []) _ = gfZero
gfMul _ (GF []) = gfZero
gfMul (GF (a0 : as)) (GF bs@(b0 : bsTail)) =
  GF (a0 * b0 : remainingCoefficients)
  where
    remainingCoefficients = zipWith (+) (map (a0 *) bsTail) recursiveProduct
    GF recursiveProduct =  gfMul (GF as) (GF bs)

-- Termwise negation.
gfNegate :: GF -> GF
gfNegate (GF coefficients) = GF (map negate coefficients)


-- Multiply each coefficient by a scalar.
gfScale :: Rational -> GF -> GF
gfScale scalar (GF coefficients) = GF (map (scalar *) coefficients)

gfPow :: GF -> Natural -> GF
gfPow _ 0 = gfOne
gfPow gf 1 = gf
gfPow gf power
  | even power =
      let half = gfPow gf (power `div` 2)
      in half * half
  | otherwise =
      gf * gfPow gf (power - 1)

-- Division

gfDivide :: GF -> GF -> Either GFError GF
gfDivide dividend divisor
  | gfConstantTerm divisor == 0 = Left DivisionByZeroConstant
  | otherwise = Right (gfDivideUnsafe dividend divisor)

gfDivideUnsafe :: GF -> GF -> GF
gfDivideUnsafe (GF dividendCoefficients) (GF divisorCoefficients) =
  GF (divideCoeffs dividendCoefficients divisorCoefficients)
  where
    divideCoeffs :: [Rational] -> [Rational] -> [Rational]
    divideCoeffs [] _ =  error "GF invariant violated: finite dividend coefficient list"
    divideCoeffs _ [] =  error "GF invariant violated: finite divisor coefficient list"

    divideCoeffs (dividendConstant : dividendTail) divisor@(divisorConstant : divisorTail) =
        quotientConstant : divideCoeffs remainderTail divisor
        where
          quotientConstant = dividendConstant / divisorConstant
          remainderTail = zipWith (-) dividendTail (map (quotientConstant *) divisorTail)


gfReciprocal :: GF -> Either GFError GF
gfReciprocal =
  gfDivide gfOne


----------------------------------------
-- Calculus
----------------------------------------

-- Derivative: Formal derivative of a generating function
-- 
-- If A(x) = a_0 + a_1*x + a_2*x^2 + a_3*x^3 + ...
-- Then A'(x) = a_1 + 2*a_2*x + 3*a_3*x^2 + ...
-- 
gfDerivative :: GF -> GF
gfDerivative (GF []) = gfZero
gfDerivative (GF (_ : remainingCoefficients)) = GF ( zipWith (*) (map fromInteger [1 ..]) remainingCoefficients)

gfIntegral :: GF -> GF
gfIntegral (GF coefficients) = GF ( 0 : zipWith (/) coefficients  (map fromInteger [1 ..]))

-- Internal composition function used after validating the inner constant term.
--
-- It uses the recursive identity
--
-- A(B(x)) = a_0 + B(x)A_{\mathrm{tail}}(B(x)).
--
gfCompose :: GF -> GF -> Either GFError GF
gfCompose outer inner
  | innerConstant /= 0 = Left (CompositionRequiresZeroConstant innerConstant)
  | otherwise = Right (gfComposeUnsafe outer inner)
  where
    innerConstant = gfConstantTerm inner

gfComposeUnsafe :: GF -> GF -> GF
gfComposeUnsafe (GF []) _ = error "GF invariant violated: finite coefficient list"
gfComposeUnsafe (GF (constant : remainingCoefficients)) inner =
  gfConstant constant + inner * gfComposeUnsafe (GF remainingCoefficients) inner

--------------------
-- Comparison
--------------------

gfEqualUpTo :: Int -> GF -> GF -> Bool
gfEqualUpTo count gfA gfB = gfTake count gfA == gfTake count gfB

