module GFComb.Polynomial
  ( Polynomial,

    -- Construction
    polynomialFromList,
    polynomialZero,
    polynomialOne,
    polynomialVariable,

    -- Inspection
    polynomialCoefficients,
    polynomialConstantTerm,
    polynomialDegree,
    polynomialLeadingCoefficient,
    polynomialIsZero,

    -- Arithmetic
    polynomialAdd,
    polynomialSub,
    polynomialNegate,
    polynomialScale,
    polynomialMul,
    polynomialPow,

    -- Other operations
    polynomialEvaluate
  )
where

import Data.Ratio (denominator, numerator)
import Numeric.Natural (Natural)

---------------------
-- Polynomial type
--------------------

newtype Polynomial = Polynomial [Rational]
  deriving (Eq)


instance Show Polynomial where
  show (Polynomial coefficients) =
    case nonZeroTerms of
        [] -> "0"
        firstTerm : remainingTerms -> showLeadingTerm firstTerm ++ concatMap showFollowingTerm remainingTerms
    where
      nonZeroTerms :: [(Int, Rational)]
      nonZeroTerms = [ (degree, coefficient) | (degree, coefficient) <- zip [0 ..] coefficients, coefficient /= 0]

      showLeadingTerm :: (Int, Rational) -> String
      showLeadingTerm (degree, coefficient)
        | coefficient < 0 = "-" ++ showUnsignedTerm degree (abs coefficient)
        | otherwise = showUnsignedTerm degree coefficient

      showFollowingTerm :: (Int, Rational) -> String
      showFollowingTerm (degree, coefficient)
        | coefficient < 0 = " - " ++ showUnsignedTerm degree (abs coefficient)
        | otherwise = " + " ++ showUnsignedTerm degree coefficient

      showUnsignedTerm :: Int -> Rational -> String
      showUnsignedTerm degree coefficient =
        case degree of
          0 -> showRational coefficient

          1
            | coefficient == 1 -> "x"
            | otherwise -> showRational coefficient ++ "x"

          _
            | coefficient == 1 -> "x^" ++ show degree
            | otherwise -> showRational coefficient ++ "x^" ++ show degree

      showRational :: Rational -> String
      showRational value
        | denominator value == 1 = show (numerator value)
        | otherwise = "("++ show (numerator value) ++ "/" ++ show (denominator value) ++ ")"

-- Arithmetic syntax for polynomials.
instance Num Polynomial where
  (+) = polynomialAdd
  (-) = polynomialSub
  (*) = polynomialMul
  negate = polynomialNegate
  fromInteger = polynomialFromList . pure . fromInteger

  abs _ = error "abs is not defined for polynomials"

  signum _ = error "signum is not defined for polynomials"

----------------------------------
-- Construction
------------------------------------

--
-- produce the same polynomial.
polynomialFromList :: [Rational] -> Polynomial
polynomialFromList = Polynomial . normalizeCoefficients

-- The zero polynomial.
polynomialZero :: Polynomial
polynomialZero = Polynomial []

-- The constant polynomial one.
polynomialOne :: Polynomial
polynomialOne = Polynomial [1]

-- The polynomial representing the variable x.
polynomialVariable :: Polynomial
polynomialVariable = Polynomial [0, 1]

--------------------------------------------------------------------------------
-- Inspection
--------------------------------------------------------------------------------

-- Return the normalized finite coefficient list.
--
-- The coefficients are returned in ascending order of degree.
polynomialCoefficients :: Polynomial -> [Rational]
polynomialCoefficients (Polynomial coefficients) =
  coefficients

-- Return the degree of a polynomial.
--
-- The zero polynomial has no mathematically defined degree, so this function
-- returns 'Nothing' for zero.
--
-- 
-- polynomialDegree (polynomialFromList [1, 2, 3]) == Just 2
-- polynomialDegree polynomialZero                  == Nothing
-- 
polynomialDegree :: Polynomial -> Maybe Int
polynomialDegree (Polynomial []) =
  Nothing
polynomialDegree (Polynomial coefficients) =
  Just (length coefficients - 1)

-- Return the leading coefficient.
--
-- The zero polynomial has no leading coefficient.
polynomialLeadingCoefficient :: Polynomial -> Maybe Rational
polynomialLeadingCoefficient (Polynomial []) =
  Nothing
polynomialLeadingCoefficient (Polynomial coefficients) =
  Just (last coefficients)


polynomialConstantTerm :: Polynomial -> Rational
polynomialConstantTerm (Polynomial []) = 0
polynomialConstantTerm (Polynomial (constant : _)) = constant


-- Test whether a polynomial is the zero polynomial.
polynomialIsZero :: Polynomial -> Bool
polynomialIsZero (Polynomial coefficients) =
  null coefficients

----------------------
-- Arithmetic
----------------------

-- Add two polynomials coefficient by coefficient.
polynomialAdd :: Polynomial -> Polynomial -> Polynomial
polynomialAdd
  (Polynomial coefficientsA)
  (Polynomial coefficientsB) =
    polynomialFromList
      (zipWithLonger (+) coefficientsA coefficientsB)

-- Subtract one polynomial from another coefficient by coefficient.
polynomialSub :: Polynomial -> Polynomial -> Polynomial
polynomialSub
  (Polynomial coefficientsA)
  (Polynomial coefficientsB) =
    polynomialFromList
      (zipWithLonger (-) coefficientsA coefficientsB)

-- Negate every coefficient.
polynomialNegate :: Polynomial -> Polynomial
polynomialNegate (Polynomial coefficients) =
  Polynomial (map negate coefficients)

-- Multiply every coefficient by a rational scalar.
polynomialScale :: Rational -> Polynomial -> Polynomial
polynomialScale scalar _
  | scalar == 0 = polynomialZero
polynomialScale scalar (Polynomial coefficients) =
  polynomialFromList (map (scalar *) coefficients)

-- Multiply two polynomials using the Cauchy product.
--

polynomialMul :: Polynomial -> Polynomial -> Polynomial
polynomialMul polynomialA polynomialB
  | polynomialIsZero polynomialA = polynomialZero
  | polynomialIsZero polynomialB = polynomialZero
polynomialMul
  (Polynomial coefficientsA)
  (Polynomial coefficientsB) =
    polynomialFromList [ coefficientAt degree | degree <- [0 .. maximumDegree]]
    where
      maximumDegree = length coefficientsA + length coefficientsB - 2

      coefficientAt :: Int -> Rational
      coefficientAt degree =
        sum
          [ coefficientA * coefficientB
          | (indexA, coefficientA) <- zip [0 :: Int ..] coefficientsA,
            let indexB = degree - indexA,
            indexB >= 0,
            indexB < length coefficientsB,
            let coefficientB = coefficientsB !! indexB
          ]

-- Raise a polynomial to a non-negative integer power.
-- Exponentiation by squaring is used.
polynomialPow :: Polynomial -> Natural -> Polynomial
polynomialPow _ 0 = polynomialOne
polynomialPow polynomial 1 = polynomial
polynomialPow polynomial power
  | even power =
      let halfPower =
            polynomialPow polynomial (power `div` 2)
       in halfPower * halfPower
  | otherwise =
      polynomial * polynomialPow polynomial (power - 1)

----------------------
-- Evaluation
----------------------
-- Evaluate a polynomial at a rational value.
--
-- Horner's method:
--

polynomialEvaluate :: Polynomial -> Rational -> Rational
polynomialEvaluate (Polynomial coefficients) value =
    foldr
      (\coefficient accumulated -> coefficient + value * accumulated)
      0
      coefficients

---------------------
-- Helpers
----------------------

-- Remove trailing zeros from a coefficient list.
--

normalizeCoefficients :: [Rational] -> [Rational]
normalizeCoefficients = reverse . dropWhile (== 0) . reverse


type Operation = Rational -> Rational -> Rational

zipWithLonger :: Operation -> [Rational] -> [Rational] -> [Rational]
zipWithLonger _ [] [] = []
zipWithLonger operation (x : xs) [] = operation x 0 : zipWithLonger operation xs []
zipWithLonger operation [] (y : ys) = operation 0 y : zipWithLonger operation [] ys
zipWithLonger operation (x : xs) (y : ys) = operation x y : zipWithLonger operation xs ys