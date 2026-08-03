-- | Homogeneous linear recurrences with constant coefficients: building
-- their associated generating function, reading off terms, and -- where
-- the characteristic polynomial allows it -- an exact closed form for
-- a(n).
module GFComb.Recurrence
    ( -- * Types
        LinearRecurrence,

        -- * Construction
        linearRecurrence,

        -- * Inspection
        recurrenceOrder,
        recurrenceCoefficients,
        recurrenceInitialValues,

        -- * Generating-function construction
        recurrenceNumerator,
        recurrenceDenominator,
        recurrenceRationalGF,
        recurrenceGF,

        -- * Terms
        recurrenceTerms,
        recurrenceTermAt,

        -- * Closed form
        Surd,
        ClosedFormTerm (..),
        ClosedFormResult (..),
        recurrenceClosedForm,
        showClosedForm,
        closedFormValueAt
    )
        where

import GFComb.Core ( GF, gfCoeffAtMaybe, gfTake)
import GFComb.Polynomial
    ( Polynomial,
      polynomialFromList,
      polynomialCoefficients,
      polynomialDegree,
      polynomialExtractRationalRoots
    )
import GFComb.RationalGF ( RationalGF, rationalGF, rationalGFToGF)

import Data.List (foldl', intercalate, tails)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio (numerator, denominator, (%))

------------------------------
-- Linear recurrences
--------------------------------

-- | Represents
--
-- > a_n = c1 * a_(n-1) + c2 * a_(n-2) + ... + ck * a_(n-k)
--
-- with initial values @[a_0, a_1, ..., a_(k-1)]@ and coefficients
-- @[c1, c2, ..., ck]@.
--
-- Pairing the coefficients and initial values together, one list instead
-- of two, means a recurrence's coefficients and initial values are
-- always exactly the same length by construction, and there is always at
-- least one of each (thanks to 'Data.List.NonEmpty'). This is what lets
-- 'linearRecurrence' below be a total function: the two ways
-- construction used to fail at runtime (an empty recurrence, or a
-- coefficient\/initial-value count mismatch) are now ruled out by the
-- type of its argument instead of checked afterwards.
--
-- The constructor is hidden from users of the module. Valid recurrence
-- values must be created with 'linearRecurrence'.
newtype LinearRecurrence = LinearRecurrence (NonEmpty (Rational, Rational))
    deriving (Eq, Show)

--------------------------
-- Construction
-------------------------------

-- | Construct a homogeneous linear recurrence with constant coefficients,
-- from a non-empty list of (coefficient, initial value) pairs.
--
-- For example, @linearRecurrence ((1, 1) :| [(1, 1)])@ represents
--
-- > a_n = a_(n-1) + a_(n-2),  a_0 = 1,  a_1 = 1
--
-- Because the coefficients and initial values are given together, one
-- per pair, this can no longer be called with mismatched lengths or with
-- no coefficients at all, so (unlike the previous, two-separate-list version) this can
-- no longer fail.
--
-- >>> map numerator (recurrenceTerms 10 (linearRecurrence (NonEmpty.fromList [(1, 1), (1, 1)])))
-- [1,1,2,3,5,8,13,21,34,55]
linearRecurrence :: NonEmpty (Rational, Rational) -> LinearRecurrence
linearRecurrence = LinearRecurrence

-------------------
-- Inspection
-----------------------

-- | The coefficients [c1, c2, ..., ck].
recurrenceCoefficients :: LinearRecurrence -> [Rational]
recurrenceCoefficients (LinearRecurrence pairs) = map fst (NonEmpty.toList pairs)

-- | The initial values [a_0, a_1, ..., a_(k-1)].
recurrenceInitialValues :: LinearRecurrence -> [Rational]
recurrenceInitialValues (LinearRecurrence pairs) = map snd (NonEmpty.toList pairs)

-- | Return the order of the recurrence.
recurrenceOrder :: LinearRecurrence -> Int
recurrenceOrder recurrence = length (recurrenceCoefficients recurrence)
 
----------------------------------------
-- Generating-function construction
-------------------------------------

-- | Construct the denominator of the recurrence generating function.
--
-- For @a_n = c1*a_(n-1) + ... + ck*a_(n-k)@, the denominator is
--
-- > Q(x) = 1 - c1*x - c2*x^2 - ... - ck*x^k.
recurrenceDenominator :: LinearRecurrence -> Polynomial
recurrenceDenominator recurrence =
    polynomialFromList (1 : map negate coefficients)
    where
        coefficients = recurrenceCoefficients recurrence

-- | Construct the numerator of the recurrence generating function.
--
-- If @Q(x) = 1 - c1*x - ... - ck*x^k@, then the numerator contains the
-- first k coefficients of @Q(x)*A(x)@. The coefficient of degree n is
--
-- > a_n - c1*a_(n-1) - c2*a_(n-2) - ... - cn*a_0.
recurrenceNumerator :: LinearRecurrence -> Polynomial
recurrenceNumerator recurrence =
    polynomialFromList  [ numeratorCoefficient degree initialValue | 
                          (degree, initialValue) <- zip [0 ..] initialValues ]
    where
        coefficients = recurrenceCoefficients recurrence
        initialValues =  recurrenceInitialValues recurrence
 
        numeratorCoefficient :: Int -> Rational -> Rational
        numeratorCoefficient degree initialValue =
            initialValue - sum ( zipWith (*) (take degree coefficients) (reverse (take degree initialValues)))

-- | Construct the rational generating function associated with a recurrence.
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

-- | Expand the recurrence's rational generating function as an infinite
-- formal power series.
recurrenceGF :: LinearRecurrence -> GF
recurrenceGF = rationalGFToGF . recurrenceRationalGF

-----------------------------------
-- Terms
-----------------------------------

-- | Return the requested number of terms of the recurrence.
--
-- A non-positive count produces an empty list.
recurrenceTerms :: Int -> LinearRecurrence -> [Rational]
recurrenceTerms count recurrence = gfTake count (recurrenceGF recurrence)

-- | Return the term at the given zero-based index.
--
-- A negative index returns 'Nothing'.
recurrenceTermAt :: Int -> LinearRecurrence -> Maybe Rational
recurrenceTermAt index recurrence = gfCoeffAtMaybe (recurrenceGF recurrence) index

----------------------------------------
-- Closed form: an exact p + q*sqrt(d) number type
----------------------------------------
-- | An element of Q(sqrt d), for some fixed integer d, represented as
--
-- > p + q * sqrt(d)
--
-- This is exactly the kind of number that shows up as a root of a
-- quadratic (or higher, after factoring out rational roots) characteristic
-- polynomial with rational coefficients: for example the roots of
-- x^2 - x - 1 (Fibonacci) are (1 +- sqrt(5)) \/ 2, elements of Q(sqrt 5).
--
-- d may be negative, giving a complex quadratic extension (e.g. d = -1
-- gives the Gaussian rationals); the arithmetic below is independent of the sign of d.
--
-- Used to represent roots of the characteristic polynomial and their
-- partial-fraction coefficients exactly, for recurrences whose
-- characteristic polynomial factors into rational roots plus at most one
-- irreducible quadratic factor (e.g. Fibonacci's golden-ratio pair).
--
-- A Surd with surdIrrational == 0 represents an ordinary rational number.
-- Because this module supports at most one irreducible quadratic factor per recurrence, 
-- every genuinely irrational Surd appearing together in a
-- single computation shares the same radicand. Combining two Surds with different,
-- both-nonzero irrational parts is a programming error in this module: it
-- would require a genuinely larger algebraic number field, which is out of
-- scope. Every root used in this project's recurrence-solving code comes
-- from a single characteristic polynomial's single irreducible quadratic
-- factor, so this situation should never arise in practice.
--
-- Surd d p q represents p + q * sqrt(d).
data Surd = Surd Integer Rational Rational

instance Eq Surd where
  (Surd _ p1 0) == (Surd _ p2 0) = p1 == p2
  (Surd d1 p1 q1) == (Surd d2 p2 q2) = q1 /= 0 && q2 /= 0 && d1 == d2 && p1 == p2 && q1 == q2

instance Show Surd where
  show = showSurd

surdFromRational :: Rational -> Surd
surdFromRational r = Surd 1 r 0

surdAdd :: Surd -> Surd -> Surd
surdAdd (Surd _ p1 0) (Surd d2 p2 q2) = Surd d2 (p1 + p2) q2
surdAdd (Surd d1 p1 q1) (Surd _ p2 0) = Surd d1 (p1 + p2) q1
surdAdd (Surd d1 p1 q1) (Surd d2 p2 q2)
  | d1 == d2 = Surd d1 (p1 + p2) (q1 + q2)
  | otherwise = error "Surd: mismatched radicands"

surdNegate :: Surd -> Surd
surdNegate (Surd d p q) = Surd d (negate p) (negate q)

surdSub :: Surd -> Surd -> Surd
surdSub a b = surdAdd a (surdNegate b)

surdMul :: Surd -> Surd -> Surd
surdMul (Surd _ p1 0) (Surd d2 p2 q2) = Surd d2 (p1 * p2) (p1 * q2)
surdMul (Surd d1 p1 q1) (Surd _ p2 0) = Surd d1 (p1 * p2) (p2 * q1)
surdMul (Surd d1 p1 q1) (Surd d2 p2 q2)
  | d1 == d2 = Surd d1 (p1*p2 + q1*q2 * fromInteger d1) (p1*q2 + p2*q1)
  | otherwise = error "Surd: mismatched radicands"

surdRecip :: Surd -> Surd
surdRecip (Surd d p 0) = Surd d (recip p) 0
surdRecip (Surd d p q) = Surd d (p / normSquared) (negate q / normSquared)
  where
    normSquared = p * p - q * q * fromInteger d

surdDiv :: Surd -> Surd -> Surd
surdDiv a b = surdMul a (surdRecip b)

surdPow :: Surd -> Int -> Surd
surdPow _ 0 = surdFromRational 1
surdPow surd power
  | power < 0 = surdRecip (surdPow surd (negate power))
  | even power =
      let half = surdPow surd (power `div` 2)
       in surdMul half half
  | otherwise = surdMul surd (surdPow surd (power - 1))

-- The exact non-negative square root of a non-negative rational, expressed
-- as a Surd with a square-free radicand.
--
-- sqrt(p/q) = sqrt(p*q) / q, and the largest perfect-square factor is then
-- pulled out of p*q so the radicand is as small as possible
-- (e.g. sqrt(20) is returned as 2*sqrt(5), not sqrt(20)).
exactSqrtRational :: Rational -> Surd
exactSqrtRational delta = Surd squarefreePart 0 (squareFactor % denominatorOfDelta)
  where
    numeratorOfDelta = numerator delta
    denominatorOfDelta = denominator delta
    (squareFactor, squarefreePart) = extractSquareFactor (numeratorOfDelta * denominatorOfDelta)

-- Split n = s^2 * m, with m left as square-free as trial division up to
-- sqrt(n) can make it, returning (s, m).
extractSquareFactor :: Integer -> (Integer, Integer)
extractSquareFactor n = go (abs n) 2 1
  where
    go remaining factor squareAcc
      | factor*factor > remaining = (squareAcc, remaining)
      | remaining `mod` (factor * factor) == 0 =
          go (remaining `div` (factor * factor)) factor (squareAcc * factor)
      | otherwise = go remaining (factor + 1) squareAcc

showSurd :: Surd -> String
showSurd (Surd _ p 0) = showRationalPlain p
showSurd (Surd d 0 q) = showSurdTerm q d
showSurd (Surd d p q)
  | q > 0 = showRationalPlain p ++ " + " ++ showSurdTerm q d
  | otherwise = showRationalPlain p ++ " - " ++ showSurdTerm (negate q) d

showSurdTerm :: Rational -> Integer -> String
showSurdTerm coefficient d
  | coefficient == 1 = "sqrt(" ++ show d ++ ")"
  | otherwise = showRationalPlain coefficient ++ "*sqrt(" ++ show d ++ ")"

showRationalPlain :: Rational -> String
showRationalPlain r
  | denominator r == 1 = show (numerator r)
  | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

-----------------------------
-- Closed form for a linear recurrence
---------------------------------

-- | One term A * r^n of a closed-form solution a(n) = sum of such terms.
data ClosedFormTerm = ClosedFormTerm {  termCoefficient :: Surd,
                                         -- ^ The coefficient A.
                                        termRoot :: Surd
                                         -- ^ The root r.
                                      }
        deriving (Eq, Show)

-- | The result of attempting to find a closed form for a(n).
--
-- A closed form is found exactly when the characteristic polynomial of the
-- recurrence factors, over the rationals, into distinct linear factors and
-- at most one irreducible quadratic factor. Repeated roots (which would
-- need extra n * r^n-style terms) and complex roots (which this module
-- does not represent) are reported as 'NoClosedForm', rather than silently dropped.
data ClosedFormResult = ClosedForm [ClosedFormTerm] | NoClosedForm {reasonNoClosedForm :: String}
    deriving (Eq, Show)

-- | Trying to compute the closed form a(n) = sum_i A_i * r_i^n for a linear
-- recurrence, via partial-fraction decomposition of its rational
-- generating function A(x) = P(x)\/Q(x) = sum_i A_i \/ (1 - r_i * x).
--
-- The r_i are the roots of the characteristic polynomial y^k = c1*y^(k-1)
-- + ... + ck (obtained from Q by reversing its coefficient list), found
-- first via the rational root theorem and then, for at most one leftover
-- quadratic factor, via the quadratic formula over a p + q*sqrt(d)
-- extension of the rationals.
-- The A_i are the partial-fraction residues, computed exactly as
--
-- > A_i = P(1/r_i) / product_{j /= i} (1 - r_j/r_i).
recurrenceClosedForm :: LinearRecurrence -> ClosedFormResult
recurrenceClosedForm recurrence =
  case characteristicRoots (recurrenceDenominator recurrence) of
    Left reason -> NoClosedForm reason
    Right roots
      | hasDuplicateRoot roots ->
          NoClosedForm "the characteristic polynomial has a repeated root (closed form would need an n * r^n term, which is not supported)"
      | otherwise ->
          ClosedForm
            [ ClosedFormTerm
                (partialFractionCoefficient numeratorPolynomial root (take index roots ++ drop (index + 1) roots))
                root
              | (index, root) <- zip [0 :: Int ..] roots
            ]
  where
    numeratorPolynomial = recurrenceNumerator recurrence

-- The roots of the characteristic polynomial, as Surds.
--
-- Left with an explanation whenever the polynomial does not factor into
-- rational roots plus at most one irreducible quadratic factor.
characteristicRoots :: Polynomial -> Either String [Surd]
characteristicRoots denominatorPolynomial =
  case polynomialDegree leftover of
    Nothing -> Right rationalSurds
    Just 0 -> Right rationalSurds
    Just 1 ->
      case polynomialCoefficients leftover of
        [c0, c1] -> Right (rationalSurds ++ [surdFromRational (negate c0 / c1)])
        _ -> Left "internal error: expected a linear leftover factor"
    Just 2 ->
      case quadraticSurdRoots leftover of
        Left reason -> Left reason
        Right (root1, root2) -> Right (rationalSurds ++ [root1, root2])
    Just degree ->
      Left
        ( "the characteristic polynomial has an irreducible factor of degree "
            ++ show degree
            ++ " (only rational roots plus at most one irreducible quadratic factor are supported)"
        )
  where
    characteristicPolynomial = polynomialFromList (reverse (polynomialCoefficients denominatorPolynomial))
    (rationalRoots, leftover) = polynomialExtractRationalRoots characteristicPolynomial
    rationalSurds = map surdFromRational rationalRoots

-- Solve a*y^2 + b*y + c = 0 (given as ascending coefficients [c, b, a])
-- for its two roots via the quadratic formula, over a p + q*sqrt(d)
-- extension of the rationals.
--
-- Because this is only ever called on the leftover factor after
-- 'polynomialExtractRationalRoots' has exhaustively removed every rational
-- root, its discriminant is guaranteed not to be a perfect square (a
-- perfect-square discriminant would mean rational roots existed, and they
-- would already have been found).
quadraticSurdRoots :: Polynomial -> Either String (Surd, Surd)
quadraticSurdRoots quadratic =
  case polynomialCoefficients quadratic of
    [c0, c1, c2]
      | discriminant < 0 -> Left "the characteristic polynomial has complex roots (no real closed form)"
      | discriminant == 0 -> Left "the characteristic polynomial has a repeated root (closed form would need an n * r^n term, which is not supported)"
      | otherwise -> Right (root1, root2)
      where
        discriminant = c1 * c1 - 4 * c2 * c0
        sqrtDiscriminant = exactSqrtRational discriminant
        twoA = surdFromRational (2 * c2)
        negB = surdFromRational (negate c1)
        root1 = surdDiv (surdAdd negB sqrtDiscriminant) twoA
        root2 = surdDiv (surdSub negB sqrtDiscriminant) twoA
    _ -> Left "internal error: expected a quadratic leftover factor"


hasDuplicateRoot :: [Surd] -> Bool
hasDuplicateRoot roots =
  or [ root == laterRoot | (root : laterRoots) <- tails roots, laterRoot <- laterRoots ]
 
-- The partial-fraction residue A_i = P(1/r_i) / product_{j /= i} (1 - r_j/r_i),
-- so that A(x) = sum_i A_i / (1 - r_i*x) and hence a(n) = sum_i A_i * r_i^n.
partialFractionCoefficient :: Polynomial -> Surd -> [Surd] -> Surd
partialFractionCoefficient numeratorPolynomial root otherRoots =
  surdDiv numeratorAtReciprocalRoot denominatorProduct
  where
    reciprocalRoot = surdRecip root
    numeratorAtReciprocalRoot = evaluatePolynomialAtSurd numeratorPolynomial reciprocalRoot
    denominatorProduct =
      foldl'
        surdMul
        (surdFromRational 1)
        [ surdSub (surdFromRational 1) (surdMul otherRoot reciprocalRoot) | otherRoot <- otherRoots ]

-- Evaluate a polynomial with rational coefficients at a Surd, with Horner's
-- method (same as GFComb.Polynomial.polynomialEvaluate, generalized to the
-- Surd extension field).
evaluatePolynomialAtSurd :: Polynomial -> Surd -> Surd
evaluatePolynomialAtSurd polynomial x =
  foldr   (\coefficient acc -> surdAdd (surdFromRational coefficient) (surdMul x acc))
          (surdFromRational 0)
          (polynomialCoefficients polynomial)

-- | Render a closed form as a human-readable string.
--
-- >>> showClosedForm (recurrenceClosedForm (linearRecurrence (NonEmpty.fromList [(1, 1), (1, 1)])))
-- "a(n) = (1/2 + 1/10*sqrt(5)) * (1/2 + 1/2*sqrt(5))^n + (1/2 - 1/10*sqrt(5)) * (1/2 - 1/2*sqrt(5))^n"
showClosedForm :: ClosedFormResult -> String
showClosedForm (NoClosedForm reason) = "No closed form available: " ++ reason
showClosedForm (ClosedForm terms) = "a(n) = " ++ intercalate " + " (map showTerm terms)
  where
    showTerm term = "(" ++ showSurd (termCoefficient term) ++ ") * (" ++ showSurd (termRoot term) ++ ")^n"

-- | Evaluate a closed form at a specific n, to cross-check it against
-- 'recurrenceTermAt'. Returns 'Nothing' if there is no closed form.
closedFormValueAt :: ClosedFormResult -> Int -> Maybe Rational
closedFormValueAt (NoClosedForm _) _ = Nothing
closedFormValueAt (ClosedForm terms) n =
  case total of
    Surd _ value 0 -> Just value
    _ -> Nothing
  where
    total = foldl' surdAdd (surdFromRational 0) [ surdMul (termCoefficient term) (surdPow (termRoot term) n) | term <- terms ]