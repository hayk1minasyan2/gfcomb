-- | Rational generating functions: a formal power series expressed as
-- the quotient of two finite polynomials, P(x)\/Q(x), together with the
-- one precondition (Q's constant term is nonzero) that makes the
-- quotient's power-series expansion well-defined and unique.
module GFComb.RationalGF
    ( -- * Rational generating functions
      RationalGF,
      RationalGFError (..),

      -- * Construction
      rationalGF,

      rationalGFNumerator,
      rationalGFDenominator,

      -- * Conversion
      rationalGFToGF

    )
      where

import GFComb.Polynomial ( Polynomial, polynomialConstantTerm, polynomialOne, polynomialDegree)
import GFComb.Conversion (polynomialToGF)
import GFComb.Core (GF, gfDivide)


-- | Displays a rational GF as @numerator \/ (denominator)@, e.g.
-- @1 + x \/ (1 - x - x^2)@; when the denominator is exactly 1, only the
-- numerator is shown.
--
-- >>> fmap show (rationalGF polynomialOne polynomialOne)
-- Right "1"
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
-- | A rational generating function represented as a quotient
--
-- > P(x)/Q(x)
--
-- where both P and Q are finite polynomials.
--
-- The denominator must have a non-zero constant term. This guarantees that
-- the quotient has a unique expansion as a formal power series.
data RationalGF = RationalGF {  rationalGFNumerator :: Polynomial, 
                                 -- ^ The numerator, P.
                                rationalGFDenominator :: Polynomial }
                                 -- ^ The denominator, Q. Its constant
                                 -- term is guaranteed nonzero.
    deriving (Eq)

----------------------
-- Errors
----------------------

-- | The one way constructing a rational generating function can fail:
-- the denominator's constant term was 0, so the quotient has no
-- well-defined power-series expansion.
data RationalGFError = DenominatorHasZeroConstantTerm
    deriving (Eq, Show)

----------------------------
-- Construction
-------------------------


-- | Construct a rational generating function. The denominator must have
-- a non-zero constant coefficient, or this returns
-- 'DenominatorHasZeroConstantTerm'.
rationalGF :: Polynomial -> Polynomial -> Either RationalGFError RationalGF
rationalGF numerator denominator
  | polynomialConstantTerm denominator == 0 = Left DenominatorHasZeroConstantTerm
  | otherwise = Right (RationalGF numerator denominator)


--------------------------------
-- Conversion
--------------------------------

-- | Expand a rational generating function as an infinite formal power series.
rationalGFToGF :: RationalGF -> GF
rationalGFToGF (RationalGF numerator denominator) =
  case  gfDivide (polynomialToGF numerator) (polynomialToGF denominator)
    of
      Right result -> result
      Left err -> error ( "RationalGF invariant violated during conversion: "
                            ++ show err )