module GFComb.Polynomial
  ( Polynomial,

    -- Construction
    polynomialFromList,
    polynomialZero,
    polynomialOne,
    polynomialVariable,

    -- Inspection
    polynomialCoefficients,
    polynomialCoefficientAt,
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
    polynomialEvaluate,

    -- Root-finding
    polynomialFindRationalRoot,
    polynomialExtractRationalRoots,
    polynomialDivideByLinearRoot
  )
where

import Data.Ratio (denominator, numerator, (%))
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
polynomialCoefficients (Polynomial coefficients) = coefficients

-- The coefficient of degree 'index' in a polynomial, or 0 beyond its
-- degree (similar to how a GF behaves, since Polynomial's own coefficient
-- list is normalized to drop trailing zeros).
polynomialCoefficientAt :: Polynomial -> Int -> Rational
polynomialCoefficientAt polynomial index
  | index < 0 = 0
  | otherwise = case drop index (polynomialCoefficients polynomial) of
      (coefficient : _) -> coefficient
      [] -> 0

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
polynomialDegree (Polynomial []) = Nothing
polynomialDegree (Polynomial coefficients) = Just (length coefficients - 1)

-- Return the leading coefficient.
--
-- The zero polynomial has no leading coefficient.
polynomialLeadingCoefficient :: Polynomial -> Maybe Rational
polynomialLeadingCoefficient (Polynomial []) = Nothing
polynomialLeadingCoefficient (Polynomial coefficients) = Just (last coefficients)


polynomialConstantTerm :: Polynomial -> Rational
polynomialConstantTerm (Polynomial []) = 0
polynomialConstantTerm (Polynomial (constant : _)) = constant


-- Test whether a polynomial is the zero polynomial.
polynomialIsZero :: Polynomial -> Bool
polynomialIsZero (Polynomial coefficients) = null coefficients

----------------------
-- Arithmetic
----------------------

-- Add two polynomials coefficient by coefficient.
polynomialAdd :: Polynomial -> Polynomial -> Polynomial
polynomialAdd
  (Polynomial coefficientsA)
  (Polynomial coefficientsB) =
    polynomialFromList (zipWithLonger (+) coefficientsA coefficientsB)

-- Subtract one polynomial from another coefficient by coefficient.
polynomialSub :: Polynomial -> Polynomial -> Polynomial
polynomialSub
  (Polynomial coefficientsA)
  (Polynomial coefficientsB) =
    polynomialFromList (zipWithLonger (-) coefficientsA coefficientsB)

-- Negate every coefficient.
polynomialNegate :: Polynomial -> Polynomial
polynomialNegate (Polynomial coefficients) = Polynomial (map negate coefficients)

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
        sum (zipWith (*) sliceA (reverse sliceB))
        where
          lowA = max 0 (degree - length coefficientsB + 1)
          lowB = max 0 (degree - length coefficientsA + 1)
          sliceA = drop lowA (take (degree + 1) coefficientsA)
          sliceB = drop lowB (take (degree + 1) coefficientsB)


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
    foldr (\coefficient acc -> coefficient + value * acc) 0 coefficients

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


----------------------
-- Root-finding
----------------------

-- Find one rational root of a polynomial with rational coefficients, via
-- the rational root theorem.
--
-- If the constant term is zero, 0 is a root and is returned immediately.
-- Otherwise the polynomial is scaled to integer coefficients (multiplying
-- all coefficients by the lcm of the coefficients' denominators (this changes
-- nothing about where the roots are)), and candidates p/q are formed from
-- divisors p of the resulting constant term and divisors q of the
-- resulting leading coefficient, then tested directly by evaluation. (fastest way to check)
--
-- Returns Nothing if the polynomial is zero, constant, or has no rational root.

polynomialFindRationalRoot :: Polynomial -> Maybe Rational
polynomialFindRationalRoot polynomial =
  case polynomialDegree polynomial of
    Nothing -> Nothing
    Just 0 -> Nothing
    Just _
      | polynomialConstantTerm polynomial == 0 -> Just 0
      | otherwise -> findFirstRoot candidates
  where
    coefficients = polynomialCoefficients polynomial

    scaleFactor :: Integer
    scaleFactor = foldr (lcm . denominator) 1 coefficients

    integerCoefficients :: [Integer]
    integerCoefficients = map (\c -> numerator (c * fromInteger scaleFactor)) coefficients

    constantTerm = head integerCoefficients
    leadingCoefficient = last integerCoefficients

    candidates :: [Rational]
    candidates = [ sign * (p % q) | p <- divisorsOf (abs constantTerm),
                                    q <- divisorsOf (abs leadingCoefficient),
                                    sign <- [1, -1]]

    findFirstRoot :: [Rational] -> Maybe Rational
    findFirstRoot [] = Nothing
    findFirstRoot (candidate : rest)
      | polynomialEvaluate polynomial candidate == 0 = Just candidate
      | otherwise = findFirstRoot rest


divisorsOf :: Integer -> [Integer]
divisorsOf 0 = [1]
divisorsOf n = [d | d <- [1 .. n], n `mod` d == 0]

-- Repeatedly extract rational roots (via the rational root theorem) until
-- none remain, returning the list of roots found - in the order
-- extracted, with a root of multiplicity k appearing k times - together
-- with whatever polynomial remains after dividing them all out.
polynomialExtractRationalRoots :: Polynomial -> ([Rational], Polynomial)
polynomialExtractRationalRoots polynomial =
  case polynomialFindRationalRoot polynomial of
    Nothing -> ([], polynomial)
    Just root ->
      let reduced = polynomialDivideByLinearRoot polynomial root
          (moreRoots, remaining) = polynomialExtractRationalRoots reduced
       in (root : moreRoots, remaining)

-- Divide a polynomial by (x - r).
--
-- This assumes r is an exact root of the polynomial (as produced by
-- polynomialFindRationalRoot); the remainder of the division is not
-- checked. Using an r that is not an exact root will produce a meaningless result.

polynomialDivideByLinearRoot :: Polynomial -> Rational -> Polynomial
polynomialDivideByLinearRoot polynomial root =
  case reverse (polynomialCoefficients polynomial) of
    [] -> polynomialZero
    (leadingCoefficient : remainingDescending) ->
      let syntSteps = scanl (\acc c -> c + root*acc) leadingCoefficient remainingDescending
          quotientDescending = init syntSteps
       in polynomialFromList (reverse quotientDescending)













