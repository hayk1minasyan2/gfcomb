module GFComb.Core
  ( GF(..), 
    gfAdd,
    gfSub,
    gfMul,
    gfCoeff,
    gfTake,
    gfFromList,
    gfDerivative,
  ) where

import Data.Ratio (Rational)
import Data.Ratio (denominator, numerator)
import Data.List (intercalate)

-- A formal power series over the rationals.
-- A(x) = a_0 + a_1*x + a_2*x^2 + ... is represented as GF [a_0, a_1, a_2, ...]
-- Examples:
--   GF [1, 2, 3, 0, 0, ...] represents 1 + 2x + 3x^2
--   GF [1, 1, 1, 1, ...] represents 1/(1-x) (geometric series)
newtype GF = GF [Rational]
  deriving (Eq)


-- Displaying a GF by showing the first 10 coefficients
instance Show GF where
  show (GF coeffs) =
    "GF [" ++ intercalate "," (map showCoeff first10) ++ suffix ++ "]"
    where
      (first10, rest) = splitAt 10 coeffs
      suffix  = if null rest then "" else ",..."
      showCoeff r =
        if denominator r == 1
        then show (numerator r)
        else show r

-- I am making GF a Num instance so that we can use +, -, * syntax
instance Num GF where
  (+) = gfAdd
  (-) = gfSub
  (*) = gfMul
  negate (GF coeffs) = GF (map negate coeffs)
  fromInteger n = GF (fromInteger n : repeat 0) -- Represents the constant series n + 0*x + 0*x^2 + ...


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


-- Coefficient extraction: Get the nth coefficient of a generating function
gfCoeff :: GF -> Int -> Rational
gfCoeff (GF coeffs) n = coeffs !! n


-- Take the first n coefficients of a generating function
gfTake :: Int -> GF -> [Rational]
gfTake n (GF coeffs) = take n coeffs

-- Creating a generating function from a finite list
-- The remaining coefficients are filled with zeros
gfFromList :: [Rational] -> GF
gfFromList xs = GF (xs ++ repeat 0)

-- Multiplication: The coefficient of x^n in the product A * B is given by the convolution of the coefficients:
-- (A * B)[n] = sum_{i=0}^n a_i * b_{n-i}
gfMul :: GF -> GF -> GF
gfMul (GF as) (GF bs) = GF [convolution n as bs | n <- [0..]]
  where
    convolution :: Int -> [Rational] -> [Rational] -> Rational
    convolution n as bs = sum [as !! i * bs !! (n - i) | i <- [0..n]]


-- Derivative: Formal derivative of a generating function
-- 
-- If A(x) = a_0 + a_1*x + a_2*x^2 + a_3*x^3 + ...
-- Then A'(x) = a_1 + 2*a_2*x + 3*a_3*x^2 + ...
-- 
-- In general: A'[n] = (n+1) * a_{n+1}
gfDerivative :: GF -> GF
gfDerivative (GF (_ : coeffs)) = GF [fromIntegral (n + 1) * coeffs !! n | n <- [0..]]
gfDeriv (GF []) = GF []