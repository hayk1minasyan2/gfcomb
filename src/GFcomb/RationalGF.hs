module GFComb.RationalGF
  ( RationalGF,
    RationalGFError (..),

    -- Construction
    rationalGF,

    -- Inspection
    rationalGFNumerator,
    rationalGFDenominator,

    -- Conversion
    rationalGFToGF

  )
    where

import GFComb.Polynomial ( Polynomial, polynomialConstantTerm, polynomialOne, polynomialDegree)
import GFComb.Conversion (polynomialToGF)
import GFComb.Core (GF, gfDivide)


instance Show RationalGF where
  show (RationalGF numerator denominator)
    | denominator == polynomialOne = show numerator
    | otherwise = showNumerator numerator ++ " / (" ++ show denominator ++ ")"
    where
      showNumerator :: Polynomial -> String
      showNumerator polynomial =
        case polynomialDegree polynomial of
          Nothing -> show polynomial
          Just 0 ->  show polynomial
          Just _ -> "(" ++ show polynomial ++ ")"

---------------------
-- Rational generating functions
----------------------
-- A rational generating function represented as a quotient
--     P(x)/Q(x)
--
-- where both P and Q are finite polynomials.
--
-- The denominator must have a non-zero constant term. This guarantees that
-- the quotient has a unique expansion as a formal power series.

data RationalGF = RationalGF Polynomial Polynomial
  deriving (Eq)

----------------------
-- Errors
----------------------

data RationalGFError
  = DenominatorHasZeroConstantTerm
  deriving (Eq, Show)

----------------------------
-- Construction
-------------------------


-- The denominator must have a non-zero constant coefficient.
rationalGF :: Polynomial -> Polynomial -> Either RationalGFError RationalGF
rationalGF numerator denominator
  | polynomialConstantTerm denominator == 0 =
      Left DenominatorHasZeroConstantTerm
  | otherwise =
      Right (RationalGF numerator denominator)

-------------------------------------
-- Inspection
-------------------------------------

rationalGFNumerator :: RationalGF -> Polynomial
rationalGFNumerator (RationalGF numerator _) =
  numerator

rationalGFDenominator :: RationalGF -> Polynomial
rationalGFDenominator (RationalGF _ denominator) =
  denominator


--------------------------------
-- Conversion
--------------------------------

--  Expand a rational generating function as an infinite formal power series.
rationalGFToGF :: RationalGF -> GF
rationalGFToGF (RationalGF numerator denominator) =
  case  gfDivide (polynomialToGF numerator) (polynomialToGF denominator)
    of
      Right result -> result
      Left err -> error ( "RationalGF invariant violated during conversion: "
                            ++ show err )