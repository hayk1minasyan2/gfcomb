-- | Converting between 'Polynomial' and 'GF'.
module GFComb.Conversion
  ( polynomialToGF)
    where

import GFComb.Core (GF, gfFromList)
import GFComb.Polynomial
  ( Polynomial,
    polynomialCoefficients
  )

-- | Convert a finite polynomial into a formal power series.
--
-- Missing higher-degree coefficients are filled with zeros.
polynomialToGF :: Polynomial -> GF
polynomialToGF = gfFromList . polynomialCoefficients