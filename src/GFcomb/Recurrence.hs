-- | Linear recurrences with constant coefficients, homogeneous or 
-- with a polynomial forcing term : building
-- their associated generating function, reading off terms, and -- where
-- the characteristic polynomial allows it -- an exact closed form for
-- a(n).
module GFComb.Recurrence
    ( -- * Types
        LinearRecurrence,

        -- * Construction
        linearRecurrence,
        forcedRecurrence,
        polynomialSequence,

        -- * Inspection
        recurrenceOrder,
        recurrenceCoefficients,
        recurrenceInitialValues,
        normalizeRecurrence,

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
      polynomialExtractRationalRoots,
      polynomialEvaluate,
      polynomialMul,
      polynomialPow
    )
import GFComb.RationalGF ( RationalGF, rationalGF, rationalGFToGF)

import Data.List (dropWhileEnd, foldl', intercalate)
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

-- | Construct a linear recurrence with a polynomial forcing term,
--
-- > a_n = c1*a_(n-1) + ... + ck*a_(n-k) + f(n)     for n >= k
--
-- from the same (coefficient, initial value) pairs 'linearRecurrence'
-- takes, plus f given as a polynomial in n. Only the k initial values
-- a_0 .. a_(k-1) are needed, exactly as in the homogeneous case.
--
-- The result is an ordinary 'LinearRecurrence', because an inhomogeneous
-- recurrence with a polynomial forcing term /is/ a homogeneous one of
-- higher order. Writing the recurrence as
--
-- > a_n - c1*a_(n-1) - ... - ck*a_(n-k) = f(n)
--
-- and applying the forward difference operator (delta f(n) = f(n+1) - f(n)) d+1 times, 
-- where d is f's degree, make the right-hand side 0: a polynomial of degree d is killed by
-- d+1 differences. What is left is homogeneous, with characteristic
-- polynomial chi(y)*(y-1)^(d+1) - equivalently, denominator
-- Q(x)*(1-x)^(d+1). Its order is k+d+1, and the d+1 further initial values
-- it needs are computed here from the original rule.
--
-- Everything downstream therefore works unchanged: the generating
-- function, the terms, and the closed form. The root at 1 that this
-- introduces, with multiplicity d+1, is exactly why closed forms for these
-- recurrences need the n^j factors of 'ClosedFormTerm' - which is also
-- why a forcing term of degree 1 or more gives an answer containing a
-- polynomial in n, as it should: a_n = 2*a_(n-1) + n comes out as
-- 2^(n+1) - n - 2.
--
-- A zero forcing polynomial gives 'linearRecurrence' back unchanged.
--
-- >>> recurrenceTerms 6 (forcedRecurrence (NonEmpty.fromList [(2, 1)]) (polynomialFromList [1]))
-- [1 % 1,3 % 1,7 % 1,15 % 1,31 % 1,63 % 1]
forcedRecurrence :: NonEmpty (Rational, Rational) -> Polynomial -> LinearRecurrence
forcedRecurrence pairs forcing =
  case polynomialDegree forcing of
    Nothing -> linearRecurrence pairs
    Just forcingDegree ->
      let extendedOrder = order + forcingDegree + 1
          annihilator =
            polynomialPow (polynomialFromList [1, -1]) (fromIntegral (forcingDegree + 1))
          extendedDenominator = polynomialMul denominator_ annihilator
          -- 'polynomialFromList' drops trailing zeros, so the coefficient
          -- list read back off the extended denominator can be shorter than
          -- the order it stands for. Padding restores those zeros: without
          -- it, a recurrence whose own last coefficient is zero would come
          -- out one or more orders too short, and so describe a different
          -- sequence entirely.
          extendedCoefficients =
            take
              extendedOrder
              (map negate (drop 1 (polynomialCoefficients extendedDenominator)) ++ repeat 0)
          extendedPairs = zip extendedCoefficients (take extendedOrder forcedValues)
       in case NonEmpty.nonEmpty extendedPairs of
            Just nonEmptyPairs -> linearRecurrence nonEmptyPairs
            Nothing -> linearRecurrence pairs
  where
    coefficients = map fst (NonEmpty.toList pairs)
    initialValues = map snd (NonEmpty.toList pairs)
    order = length coefficients
    denominator_ = polynomialFromList (1 : map negate coefficients)

    -- The sequence under the original inhomogeneous rule: the given initial
    -- values, then each later term from the k before it plus f(n). Defined
    -- in terms of itself, which terminates because every term it looks back
    -- at has already been produced.
    forcedValues = initialValues ++ map nextValue [order ..]
    nextValue n =
      sum (zipWith (*) coefficients (reverse (take order (drop (n - order) forcedValues))))
        + polynomialEvaluate forcing (fromIntegral n)


-- | The recurrence whose sequence is given directly by a polynomial in n:
--
-- > a_n = f(n)     for every n
--
-- with no reference to earlier terms at all.
--
-- This is the order-zero case of 'forcedRecurrence', which cannot express
-- it: that function takes a non-empty list of (coefficient, initial value)
-- pairs, so a recurrence with no coefficients has no representation there.
-- The conversion is the same one, with the base recurrence
-- gone: d+1 differences annihilate a polynomial of degree d, leaving the
-- homogeneous recurrence with characteristic polynomial (y-1)^(d+1)
-- (denominator (1-x)^(d+1)) of order d+1, whose first d+1 values are
-- f(0) through f(d).
--
-- No initial values are taken, because there are none to take: the formula
-- already fixes every term, including the first ones.
--
-- The zero polynomial gives the all-zero sequence.
--
-- >>> recurrenceTerms 6 (polynomialSequence (polynomialFromList [1, 1]))
-- [1 % 1,2 % 1,3 % 1,4 % 1,5 % 1,6 % 1]
--
-- >>> recurrenceTerms 6 (polynomialSequence (polynomialFromList [0, 0, 1]))
-- [0 % 1,1 % 1,4 % 1,9 % 1,16 % 1,25 % 1]
polynomialSequence :: Polynomial -> LinearRecurrence
polynomialSequence formula =
  case polynomialDegree formula of
    Nothing -> zeroSequence
    Just degree ->
      let order = degree + 1
          denominator_ = polynomialPow (polynomialFromList [1, -1]) (fromIntegral order)
          coefficients =
            take order (map negate (drop 1 (polynomialCoefficients denominator_)) ++ repeat 0)
          values = [polynomialEvaluate formula (fromIntegral n) | n <- [0 .. degree]]
       in case NonEmpty.nonEmpty (zip coefficients values) of
            Just pairs -> linearRecurrence pairs
            Nothing -> zeroSequence
  where
    -- a_n = 0*a_(n-1), a_0 = 0.
    zeroSequence = linearRecurrence ((0, 0) NonEmpty.:| [])


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

-- | Split a recurrence into the prefix of its initial values that its
-- coefficients do not actually determine, and the genuine recurrence
-- underneath.
--
-- A trailing zero coefficient means the recurrence does not really depend
-- on its oldest referenced term: @a_n = 3*a_(n-1) + 0*a_(n-2)@ is an
-- order-1 recurrence written padded out to order 2. Dropping those
-- trailing zeros leaves a shorter recurrence whose sequence is the
-- original one shifted forward; the values it skips past were free in the
-- padded form (nothing determined them), so they are handed back
-- separately as the prefix.
--
-- Returns 'Nothing' for the recurrence when every coefficient is zero:
-- there is no order-0 recurrence to reduce to, and the sequence is then
-- just the prefix followed by zeros.
--
-- >>> fst (normalizeRecurrence (linearRecurrence (NonEmpty.fromList [(3, 1), (0, 7)])))
-- [1 % 1]
normalizeRecurrence :: LinearRecurrence -> ([Rational], Maybe LinearRecurrence)
normalizeRecurrence recurrence = (prefix, reduced)
  where
    coefficients = recurrenceCoefficients recurrence
    initialValues = recurrenceInitialValues recurrence
    keptCoefficients = dropWhileEnd (== 0) coefficients
    prefixLength = length coefficients - length keptCoefficients
    prefix = take prefixLength initialValues
    reduced =
      fmap
        linearRecurrence
        (NonEmpty.nonEmpty (zip keptCoefficients (drop prefixLength initialValues)))

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

-- | One term of a closed-form solution.
--
-- A term is A * n^j * r^n, so that a(n) is a sum of such terms. The n^j
-- factor is what lets a repeated root be represented: a root of
-- multiplicity m contributes m terms, with j running from 0 to m - 1.
-- For a root that appears only once, j is 0 and the term is the familiar
-- A * r^n.
data ClosedFormTerm = ClosedFormTerm
  { termCoefficient :: Surd,
    -- ^ The coefficient A.
    termPower :: Int,
    -- ^ The power j of n. Zero unless the root is repeated.
    termRoot :: Surd
    -- ^ The root r.
  }
  deriving (Eq, Show)

-- | The result of attempting to find a closed form for a(n).
--
-- A closed form is found exactly when the characteristic polynomial
-- factors, over the rationals, into linear factors and at most one
-- irreducible quadratic factor. Rational roots may repeat, and are
-- handled by the n^j factors in 'ClosedFormTerm'; complex roots (which
-- this module does not represent) and a repeated /irrational/ root (which
-- would leave an irreducible factor of degree four) are reported as
-- 'NoClosedForm' rather than silently dropped.
--
-- The first field holds values for the first few n given directly rather
-- than by the sum of terms: a recurrence with trailing zero coefficients
-- has a few leading values that its own recurrence rule never determines
-- (see 'normalizeRecurrence'), and those cannot be expressed as terms.
data ClosedFormResult
  = ClosedForm [Rational] [ClosedFormTerm]
  | NoClosedForm {reasonNoClosedForm :: String}
  deriving (Eq, Show)

-- | Compute the closed form a(n) = sum_i A_i * n^(j_i) * r_i^n for a linear
-- recurrence.
--
-- The roots r_i are those of the characteristic polynomial
-- y^k = c1*y^(k-1) + ... + ck, found first via the rational root theorem
-- and then, for at most one leftover quadratic factor, via the quadratic
-- formula over a p + q*sqrt(d) extension of the rationals.
--
-- The coefficients A_i are found by solving a linear system rather than by
-- the partial-fraction residue formula. The shape of the answer is known
-- in advance -- a root of multiplicity m contributes terms in
-- n^0 * r^n through n^(m-1) * r^n, and the multiplicities sum to the
-- order k -- so there are exactly k unknowns, and the k initial values
-- give exactly k equations. Solving those directly is both simpler than
-- residues at a repeated pole (which would need derivatives) and free of
-- the shift correction a prefix would otherwise require: the equations are
-- written at the actual values of n the initial values belong to, so the
-- resulting coefficients are already expressed in terms of n.
recurrenceClosedForm :: LinearRecurrence -> ClosedFormResult
recurrenceClosedForm recurrence =
  case reducedRecurrence of
    Nothing -> ClosedForm prefix []
    Just reduced ->
      case characteristicRoots (recurrenceCoefficients reduced) of
        Left reason -> NoClosedForm reason
        Right roots ->
          let columns = termShapes roots
              initialValues = recurrenceInitialValues reduced
              rows =
                [ map (rowEntry n) columns
                  | n <- take (length initialValues) [prefixLength ..]
                ]
              augmented = zipWith (\row value -> row ++ [surdFromRational value]) rows initialValues
           in case solveLinearSystem augmented of
                Nothing ->
                  NoClosedForm
                    "the linear system for the closed form's coefficients has no unique solution"
                Just coefficients ->
                  ClosedForm
                    prefix
                    [ ClosedFormTerm coefficient power root
                      | (coefficient, (root, power)) <- zip coefficients columns,
                        not (surdIsZero coefficient)
                    ]
  where
    (prefix, reducedRecurrence) = normalizeRecurrence recurrence
    prefixLength = length prefix

    rowEntry n (root, power) =
      surdMul (surdFromRational (fromIntegral n ^ power)) (surdPow root n)

-- The shape of each term in the closed form: one (root, power) pair per
-- unknown coefficient.
--
-- A root of multiplicity m contributes m of them, with powers 0 through
-- m - 1, so the total is the order of the recurrence.
termShapes :: [Surd] -> [(Surd, Int)]
termShapes roots =
  [(root, power) | (root, multiplicity) <- groupRoots roots, power <- [0 .. multiplicity - 1]]

-- Count how often each distinct root occurs, keeping first-appearance
-- order so that the terms of a closed form come out in the same order the
-- roots were found in.
groupRoots :: [Surd] -> [(Surd, Int)]
groupRoots = foldl' countRoot []
  where
    countRoot groups root =
      case break (\(known, _) -> known == root) groups of
        (before, (known, count) : after) -> before ++ (known, count + 1) : after
        (before, []) -> before ++ [(root, 1)]

surdIsZero :: Surd -> Bool
surdIsZero (Surd _ p q) = p == 0 && q == 0

----------------------------------------
-- Solving a linear system over the Surds
----------------------------------------

-- Solve a square system by Gaussian elimination, given its augmented
-- matrix: each row is the k coefficients followed by that equation's
-- right-hand side.
--
-- 'Nothing' if the system is singular. For the systems built here that
-- should not happen.
solveLinearSystem :: [[Surd]] -> Maybe [Surd]
solveLinearSystem rows = fmap backSubstitute (eliminate rows)

-- Reduce to triangular form. Each returned row begins with a 1, and each
-- later row is one column shorter than the one before, that column having
-- been eliminated from it.
eliminate :: [[Surd]] -> Maybe [[Surd]]
eliminate [] = Just []
eliminate rows = do
  (pivotRow, otherRows) <- selectPivot rows
  case pivotRow of
    [] -> Nothing
    (pivot : pivotRest) -> do
      let normalized = map (`surdDiv` pivot) pivotRest
          eliminateFrom row =
            case row of
              (leading : rest) ->
                zipWith (\entry above -> surdSub entry (surdMul leading above)) rest normalized
              [] -> []
      remainingRows <- eliminate (map eliminateFrom otherRows)
      Just ((surdFromRational 1 : normalized) : remainingRows)

-- Find a row whose first entry is non-zero, returning it along with the
-- rows that were not chosen.
selectPivot :: [[Surd]] -> Maybe ([Surd], [[Surd]])
selectPivot = go []
  where
    go _ [] = Nothing
    go passedOver (row : rest) =
      case row of
        (leading : _)
          | not (surdIsZero leading) -> Just (row, reverse passedOver ++ rest)
        _ -> go (row : passedOver) rest

-- Read off the solution from the triangular form produced by 'eliminate',
-- last unknown first.
--
-- Each row is 1, then the coefficients of the unknowns after this one,
-- then the right-hand side -- so once those later unknowns are known, this
-- row's unknown is what is left of the right-hand side.
backSubstitute :: [[Surd]] -> [Surd]
backSubstitute = foldr solveRow []
  where
    solveRow row alreadySolved =
      case row of
        (_ : coefficientsAndRhs) ->
          case splitOffLast coefficientsAndRhs of
            Just (coefficients, rightHandSide) ->
              foldl' surdSub rightHandSide (zipWith surdMul coefficients alreadySolved)
                : alreadySolved
            Nothing -> alreadySolved
        [] -> alreadySolved

-- Split a list into its leading elements and its final one.
splitOffLast :: [a] -> Maybe ([a], a)
splitOffLast [] = Nothing
splitOffLast [final] = Just ([], final)
splitOffLast (first : rest) = do
  (leading, final) <- splitOffLast rest
  Just (first : leading, final)

-- | Render a closed form as a human-readable string.
--
-- >>> showClosedForm (recurrenceClosedForm (linearRecurrence (NonEmpty.fromList [(1, 1), (1, 1)])))
-- "a(n) = (1/2 + 1/10*sqrt(5)) * (1/2 + 1/2*sqrt(5))^n + (1/2 - 1/10*sqrt(5)) * (1/2 - 1/2*sqrt(5))^n"
showClosedForm :: ClosedFormResult -> String
showClosedForm (NoClosedForm reason) = "No closed form available: " ++ reason
showClosedForm (ClosedForm prefix terms) =
  concatMap showPrefixValue (zip [0 :: Int ..] prefix) ++ mainPart
  where
    showPrefixValue (index, value) =
      "a(" ++ show index ++ ") = " ++ showRationalPlain value ++ "; "
    mainPart
      | null terms = "a(n) = 0 for n >= " ++ show (length prefix)
      | null prefix = "a(n) = " ++ termsPart
      | otherwise = "a(n) = " ++ termsPart ++ " for n >= " ++ show (length prefix)
    termsPart = intercalate " + " (map showTerm terms)

    -- A factor of n^0 or of 1^n carries no information, so neither is
    -- printed. Dropping the latter matters for readability more than it
    -- might seem: a root of 1 is exactly what an inhomogeneous recurrence
    -- with a polynomial forcing term contributes, so without this the
    -- triangular numbers would print as
    -- "(1/2) * n * (1)^n + (1/2) * n^2 * (1)^n".
    showTerm term = intercalate " * " (coefficientPart : powerPart ++ rootPart)
      where
        coefficientPart = "(" ++ showSurd (termCoefficient term) ++ ")"
        powerPart
          | termPower term == 0 = []
          | termPower term == 1 = ["n"]
          | otherwise = ["n^" ++ show (termPower term)]
        rootPart
          | termRoot term == surdFromRational 1 = []
          | otherwise = ["(" ++ showSurd (termRoot term) ++ ")^n"]

-- | Evaluate a closed form at a specific n, to cross-check it against
-- 'recurrenceTermAt'. Returns 'Nothing' if there is no closed form.
closedFormValueAt :: ClosedFormResult -> Int -> Maybe Rational
closedFormValueAt (NoClosedForm _) _ = Nothing
closedFormValueAt (ClosedForm prefix terms) n
  | n < 0 = Nothing
  | otherwise =
      case drop n prefix of
        prefixValue : _ -> Just prefixValue
        [] ->
          case total of
            Surd _ value 0 -> Just value
            _ -> Nothing
  where
    total = foldl' surdAdd (surdFromRational 0) (map valueOfTerm terms)
    valueOfTerm term =
      surdMul
        (surdMul (termCoefficient term) (surdFromRational (fromIntegral n ^ termPower term)))
        (surdPow (termRoot term) n)

-- The roots of the characteristic polynomial, as Surds.
--
-- Left with an explanation whenever the polynomial does not factor into
-- rational roots plus at most one irreducible quadratic factor.
characteristicRoots :: [Rational] -> Either String [Surd]
characteristicRoots coefficients =
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
    characteristicPolynomial = polynomialFromList (reverse (map negate coefficients) ++ [1])
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
-- would already have been found). In particular a zero discriminant, which
-- would mean a repeated root, cannot arise here: that root would be
-- rational, and so already extracted.
quadraticSurdRoots :: Polynomial -> Either String (Surd, Surd)
quadraticSurdRoots quadratic =
  case polynomialCoefficients quadratic of
    [c0, c1, c2]
      | discriminant < 0 -> Left "the characteristic polynomial has complex roots (no real closed form)"
      | otherwise -> Right (root1, root2)
      where
        discriminant = c1 * c1 - 4 * c2 * c0
        sqrtDiscriminant = exactSqrtRational discriminant
        twoA = surdFromRational (2 * c2)
        negB = surdFromRational (negate c1)
        root1 = surdDiv (surdAdd negB sqrtDiscriminant) twoA
        root2 = surdDiv (surdSub negB sqrtDiscriminant) twoA
    _ -> Left "internal error: expected a quadratic leftover factor"