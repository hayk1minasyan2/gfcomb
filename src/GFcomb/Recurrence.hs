module GFComb.Recurrence
    ( -- Types
        LinearRecurrence,
        RecurrenceError (..),

        -- Construction
        linearRecurrence,

        -- Inspection
        recurrenceOrder,
        recurrenceCoefficients,
        recurrenceInitialValues,

        -- Generating-function construction
        recurrenceNumerator,
        recurrenceDenominator,
        recurrenceRationalGF,
        recurrenceGF,

        -- Terms
        recurrenceTerms,
        recurrenceTermAt
    )
        where

import GFComb.Core ( GF, gfCoeffAtMaybe, gfTake)
import GFComb.Polynomial ( Polynomial, polynomialFromList)
import GFComb.RationalGF ( RationalGF, rationalGF, rationalGFToGF)

------------------------------
-- Linear recurrences
--------------------------------

--
-- a_n = c1 * a_(n-1)
--     + c2 * a_(n-2)
--     + ...
--     + ck * a_(n-k).
--
-- The initial values are
--
-- [a_0, a_1, ..., a_(k-1)].
-- 
-- The coefficients are
--
-- [c1, c2, ..., c_k].
--
-- The constructor is hidden from users of the module. Valid recurrence values
-- must be created with 'linearRecurrence'.
data LinearRecurrence = LinearRecurrence
    { recurrenceCoefficients :: [Rational],
      recurrenceInitialValues :: [Rational]
    }
    deriving (Eq, Show)

-------------------
-- Errors
---------------------

-- Errors that may occur while constructing a linear recurrence.
data RecurrenceError = EmptyRecurrenceCoefficients | InitialValueCountMismatch
        { expectedInitialValueCount :: Int,
            actualInitialValueCount :: Int
        }
    deriving (Eq, Show)

--------------------------
-- Construction
-------------------------------

-- Construct a homogeneous linear recurrence with constant coefficients.
--
-- For example:
--
-- linearRecurrence [1, 1] [1, 1]
--
-- represents
--
-- a_n = a_(n-1) + a_(n-2),
-- a_0 = 1,
-- a_1 = 1.
-- 

linearRecurrence :: [Rational] -> [Rational] -> Either RecurrenceError LinearRecurrence
linearRecurrence coefficients initialValues
    | null coefficients = Left EmptyRecurrenceCoefficients
    | actualCount /= expectedCount = Left InitialValueCountMismatch
            { expectedInitialValueCount = expectedCount,
                actualInitialValueCount = actualCount
            }
    | otherwise = Right LinearRecurrence
            { recurrenceCoefficients = coefficients,
                recurrenceInitialValues = initialValues
            }
    where
        expectedCount = length coefficients
        actualCount = length initialValues

-------------------
-- Inspection
-----------------------

-- Return the order of the recurrence.
recurrenceOrder :: LinearRecurrence -> Int
recurrenceOrder recurrence = length (recurrenceCoefficients recurrence)

----------------------------------------
-- Generating-function construction
-------------------------------------

-- Construct the denominator of the recurrence generating function.
--
-- For
--
-- a_n = c1*a_(n-1) + ... + ck*a_(n-k),
--
-- the denominator is
--
-- Q(x) = 1 - c1*x - c2*x^2 - ... - ck*x^k.
-- 
recurrenceDenominator :: LinearRecurrence -> Polynomial
recurrenceDenominator recurrence =
    polynomialFromList (1 : map negate coefficients)
    where
        coefficients = recurrenceCoefficients recurrence

-- Construct the numerator of the recurrence generating function.
--
-- If
--
-- Q(x) = 1 - c1*x - ... - ck*x^k,
--
-- then the numerator contains the first k coefficients of
--  Q(x)*A(x).
--
-- The coefficient of degree n is
--
-- a_n - c1*a_(n-1) - c2*a_(n-2) - ... - cn*a_0.
-- 
recurrenceNumerator :: LinearRecurrence -> Polynomial
recurrenceNumerator recurrence =
    polynomialFromList [ numeratorCoefficient degree | degree <- [0 .. order - 1]]
    where
        coefficients = recurrenceCoefficients recurrence
        initialValues =  recurrenceInitialValues recurrence
        order = recurrenceOrder recurrence

        numeratorCoefficient :: Int -> Rational
        numeratorCoefficient degree =
            initialValues !! degree - sum ( zipWith (*) (take degree coefficients) (reverse (take degree initialValues)))

-- Construct the rational generating function associated with a recurrence.
recurrenceRationalGF :: LinearRecurrence -> RationalGF
recurrenceRationalGF recurrence =
    case  rationalGF (recurrenceNumerator recurrence) (recurrenceDenominator recurrence)
        of
        Right result -> result
        Left err ->  error
            ( "LinearRecurrence invariant violated while constructing "
                ++ "its rational generating function: "
                ++ show err
            )

-- Expand the recurrence's rational generating function as an infinite
-- formal power series.
recurrenceGF :: LinearRecurrence -> GF
recurrenceGF = rationalGFToGF . recurrenceRationalGF

-----------------------------------
-- Terms
-----------------------------------

-- Return the requested number of terms of the recurrence.
--
-- A non-positive count produces an empty list.

recurrenceTerms :: Int -> LinearRecurrence -> [Rational]
recurrenceTerms count recurrence = gfTake count (recurrenceGF recurrence)

-- Return the term at the given zero-based index.
--
-- A negative index returns 'Nothing'.
recurrenceTermAt :: Int -> LinearRecurrence -> Maybe Rational
recurrenceTermAt index recurrence = gfCoeffAtMaybe (recurrenceGF recurrence) index