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

    -- Structural / non-forcing operations
    gfShift,

    -- Square roots and generalized binomial series
    gfSqrtWithSeed,
    generalizedBinomial,

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
data GFError
  = DivisionByZeroConstant
  | CompositionRequiresZeroConstant Rational
  | InvalidSqrtSeed
      { sqrtSeedGiven :: Rational,
        sqrtSeedTargetConstantTerm :: Rational
      }
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
gfReciprocal = gfDivide gfOne


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
-- A(B(x)) = a_0 + B(x)A_{tail}(B(x)).
--
gfCompose :: GF -> GF -> Either GFError GF
gfCompose outer inner
  | innerConstant /= 0 = Left (CompositionRequiresZeroConstant innerConstant)
  | otherwise = Right (gfComposeUnsafe outer inner)
  where
    innerConstant = gfConstantTerm inner

gfComposeUnsafe :: GF -> GF -> GF
gfComposeUnsafe (GF outerCoefficients) inner = GF [ coefficientAt degree | degree <- [0 ..]]
  where
    powersOfInner :: [GF]
    powersOfInner = iterate (`gfMul` inner) gfOne
 
    coefficientAt :: Int -> Rational
    coefficientAt degree =
      sum [ outerCoefficient * gfCoeffAt powerOfInner degree
          | (outerCoefficient, powerOfInner) <- zip (take (degree + 1) outerCoefficients) powersOfInner
          ]
          
----------------------------------------
-- Structural operations
----------------------------------------

-- Multiply a formal power series by x^k, for a non-negative k.
--
-- Unlike 'gfMul gf (gfVariable ^ k)', this is a pure list operation, and the benefit is that it
-- simply prepends k zero coefficients and does not perform any arithmetic on the
-- existing coefficients at all.
--
-- This matters for more than efficiency. It is what makes it possible to
-- define a formal power series recursively in terms of itself, as required
-- for e.g. combinatorial specifications such as
--
--   c = 1 + x * c * c        (Catalan numbers)
--
--   c = gfAdd gfOne (gfMul gfVariable (gfMul c c))
--
-- does not terminate: to find c's own first coefficient, Haskell must force
-- gfVariable's first coefficient (0) times (c*c)'s first coefficient, and
-- forcing that product forces c's first coefficient again - the very thing
-- being computed. Using gfShift instead of "gfMul gfVariable" avoids the
-- problem , because prepending a zero never inspects the existing
-- coefficients:
--
--   c = gfAdd gfOne (gfShift 1 (gfMul c c))

gfShift :: Int -> GF -> GF
gfShift k _
  | k < 0 = error ("gfShift: negative shift " ++ show k)
gfShift k (GF coefficients) = GF (replicate k 0 ++ coefficients)

----------------------------------------
-- Square roots
----------------------------------------

-- Formal power series square root, computed one coefficient at a time.
--
-- Given a series A and a chosen rational square root 's' of A's constant
-- term (there are always two candidates, s and -s and the caller picks one),
-- returns the unique series R with R(0) = s and R * R = A.
--
-- From R * R = A, comparing coefficients of x^n gives
--
--   R[0] = s
--   R[n] = ( A[n] - sum_{i=1}^{n-1} R[i] * R[n-i] ) / (2 * R[0])   for n >= 1
--
-- Each R[n] depends only on strictly smaller-indexed coefficients of R, so
-- (as with gfShift above) this can be written as a direct, well-founded
-- self-reference and evaluated lazily, coefficient by coefficient. This is
-- the formal-power-series square root recurrence. It is the same
-- fixed-point idea behind Newton's method for R^2 - A = 0, specialised to
-- extend one coefficient at a time rather than doubling precision in
-- blocks.
gfSqrtWithSeed :: Rational -> GF -> Either GFError GF
gfSqrtWithSeed seed a_x
  | seed == 0 = Left (InvalidSqrtSeed seed constantTerm)
  | seed*seed /= constantTerm = Left (InvalidSqrtSeed seed constantTerm)
  | otherwise = Right (GF rCoefficients)
  where
    constantTerm = gfConstantTerm a_x
    aCoefficients = gfCoefficients a_x

    rCoefficients :: [Rational]
    rCoefficients = seed : [coefficientAt n | n <- [1 ..]]

    coefficientAt :: Int -> Rational
    coefficientAt n =
      (aCoefficients !! n - crossTerms n) / (2 * seed)

    crossTerms :: Int -> Rational
    crossTerms n = sum [ (rCoefficients !! i) * (rCoefficients !! (n - i)) | i <- [1 .. n - 1]]

----------------------------------------
-- Generalized binomial series
----------------------------------------

-- The generalized binomial series (1 + x)^r, for a rational exponent r.
--
-- The n-th coefficient is the generalized binomial coefficient
--
--   C(r, n) = r (r-1) (r-2) ... (r-n+1) / n!
--
-- computed via the recurrence C(r,0) = 1, C(r,n+1) = C(r,n) * (r-n) / (n+1).
--
-- Composing this with a series u where u(0) = 0 (via gfCompose) computes
-- (1+u)^r for any such u; in particular this gives an independent way to
-- compute square roots of series of the form 1 + c*x, and is the tool
-- referred to as the "generalized binomial formula" for extracting
-- explicit coefficients from algebraic generating functions such as
-- (1 - sqrt(1 - 4x)) / (2x). (see lecture "Dvořák, Z. (2026). *KG1 Notes* (Combinatorics 1 lecture notes). Charles University.")
generalizedBinomial :: Rational -> GF
generalizedBinomial r = GF (coefficientsFrom 0 1)
  where
    coefficientsFrom :: Integer -> Rational -> [Rational]
    coefficientsFrom n current =
      current : coefficientsFrom (n + 1) next
      where
        next = current * (r - fromInteger n) / fromInteger (n + 1)

--------------------
-- Comparison
--------------------

gfEqualUpTo :: Int -> GF -> GF -> Bool
gfEqualUpTo count gfA gfB = gfTake count gfA == gfTake count gfB