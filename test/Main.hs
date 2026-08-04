{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Ratio ((%))
import GFComb.Core
import GFComb.Polynomial
import GFComb.Conversion
import GFComb.RationalGF
import GFComb.Recurrence
import GFComb.AlgebraicGF
import GFComb.Builtins
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    NonNegative (..),
    Property,
    forAll,
    listOf,
    suchThat,
    property,
    (===)
  )

main :: IO ()
main =
  defaultMain $
    testGroup
      "GFComb"
      [ testGroup
          "Core"
          [ testConstruction,
            testAddition,
            testMultiplication,
            testDerivative,
            testIntegral,
            testDivision,
            testComposition,
            testErrors,
            testInfiniteMultiplication
          ],
        testGroup "Polynomial" [testPolynomial],
        testGroup "Conversion" [testPolynomialConversion],
        testGroup "RationalGF" [testRationalGFConstruction, testRationalGFConversion],
        testGroup
          "Recurrence"
          [ testRecurrenceGeneratingFunction,
            testRecurrenceTerms,
            testThirdOrderRecurrence
          ],
        testGroup "Builtins" [testBuiltins],
        testGroup
          "Core primitives (gfShift, gfSqrtWithSeed, generalizedBinomial)"
          [ testGfShift,
            testCatalanViaSelfReference,
            testGfSqrtWithSeed,
            testGfSqrtErrors,
            testGeneralizedBinomial,
            testCatalanClosedFormMatchesSelfReference
          ],
        testGroup
          "Closed form for linear recurrences"
          [ testRecurrenceClosedFormFibonacci,
            testRecurrenceClosedFormAllRationalRoots,
            testRecurrenceClosedFormComplexRootsUnavailable,
            testRecurrenceClosedFormRepeatedRootUnavailable
          ],
        testGroup
          "AlgebraicGF: guarded self-reference and Lagrange inversion"
          [ testSolveEquationCatalan,
            testSolveEquationTernaryTrees,
            testLagrangeInversionMatchesCatalan,
            testLagrangeInversionMatchesTernaryTrees,
            testLagrangeInversionCustomCubic,
            testLagrangeInversionMatchesMixedEquation,
            testAsLagrangeFormRefusesHigherXPower
          ],
        testGroup
          "Closed form for equations quadratic in the unknown"
          [ testAlgebraicClosedFormCatalan,
            testAlgebraicClosedFormBranchSelection,
            testAlgebraicClosedFormRejectsWrongY0,
            testAsQuadraticInYRefusesNonQuadratic
          ],
        testGroup
          "Property-based tests (QuickCheck)"
          [ testGfAddCommutative,
            testGfAddAssociative,
            testGfMulCommutative,
            testGfMulIdentity,
            testGfAddIdentity,
            testGfIntegralDerivative,
            testPolynomialAddCommutative,
            testPolynomialMulCommutative,
            testPolynomialMulAssociative,
            testPolynomialEvaluateDistributesOverAdd,
            testOrder1ClosedFormMatchesTerm
          ]
      ]

testConstruction :: TestTree
testConstruction =
  testCase "gfFromList pads with zeros" $
    assertEqual
      "gfFromList pads with zeros"
      [1, 2, 3, 0, 0, 0, 0, 0, 0, 0]
      (gfTake 10 (gfFromList [1, 2, 3]))

testAddition :: TestTree
testAddition =
  testCase "addition" $
    assertEqual
      "addition"
      [4, 6, 3, 0, 0]
      ( gfTake 5 $
          gfFromList [1, 2, 3] + gfFromList [3, 4]
      )

testMultiplication :: TestTree
testMultiplication =
  testCase "multiplication" $
    assertEqual
      "multiplication"
      [3, 10, 8, 0, 0]
      ( gfTake 5 $
          gfFromList [1, 2] * gfFromList [3, 4]
      )

testDerivative :: TestTree
testDerivative =
  testCase "derivative" $
    assertEqual
      "derivative"
      [2, 6, 12, 0, 0]
      (gfTake 5 (gfDerivative (gfFromList [1, 2, 3, 4])))

testIntegral :: TestTree
testIntegral =
  testCase "integral" $
    assertEqual
      "integral"
      [0, 2, 3, 4, 0]
      (gfTake 5 (gfIntegral (gfFromList [2, 6, 12])))

testDivision :: TestTree
testDivision = testCase "geometric-series division" $ do
  let x = gfVariable

  case gfDivide gfOne (gfOne - x) of
    Left err -> assertFailure ("division returned " ++ show err)
    Right result ->
      assertEqual
        "geometric-series division"
        [1, 1, 1, 1, 1, 1]
        (gfTake 6 result)

testComposition :: TestTree
testComposition = testCase "composition" $ do
  let x = gfVariable
      outer = gfFromList [2, 3, 4]
      inner = x + x * x

  case gfCompose outer inner of
    Left err -> assertFailure ("composition returned " ++ show err)
    Right result ->
      assertEqual
        "composition"
        [2, 3, 7, 8, 4, 0]
        (gfTake 6 result)

testErrors :: TestTree
testErrors = testCase "error cases" $ do
  case gfDivide gfOne gfVariable of
    Left DivisionByZeroConstant -> pure ()
    Left otherError ->
      assertFailure
        ( "division by a series with zero constant term: expected DivisionByZeroConstant, got "
            ++ show otherError
        )
    Right result ->
      assertFailure
        ( "division by a series with zero constant term: expected an error, but received "
            ++ show result
        )

  case gfCompose gfVariable (gfConstant 2) of
    Left (CompositionRequiresZeroConstant constant)
      | constant == 2 -> pure ()
    Left otherError ->
      assertFailure
        ( "composition requires zero inner constant term: expected CompositionRequiresZeroConstant 2, got "
            ++ show otherError
        )
    Right result ->
      assertFailure
        ( "composition requires zero inner constant term: expected an error, but received "
            ++ show result
        )

testInfiniteMultiplication :: TestTree
testInfiniteMultiplication = testCase "multiplication of infinite geometric series" $ do
  let x = gfVariable

  case gfReciprocal (gfOne - x) of
    Left err -> assertFailure ("infinite-series multiplication setup returned " ++ show err)
    Right geometric ->
      assertEqual
        "multiplication of infinite geometric series"
        [1, 2, 3, 4, 5, 6]
        (gfTake 6 (geometric * geometric))

testPolynomial :: TestTree
testPolynomial = testCase "polynomial arithmetic and inspection" $ do
  let x = polynomialVariable

  assertEqual
    "polynomial construction"
    [1, 2, 3]
    (polynomialCoefficients (polynomialFromList [1, 2, 3]))

  assertEqual
    "polynomial normalization"
    [1, 2, 3]
    (polynomialCoefficients (polynomialFromList [1, 2, 3, 0, 0]))

  assertEqual
    "polynomial addition"
    [4, 6, 3]
    ( polynomialCoefficients $
        polynomialFromList [1, 2, 3]
          + polynomialFromList [3, 4]
    )

  assertEqual
    "polynomial subtraction"
    [0, 0, 3]
    ( polynomialCoefficients $
        polynomialFromList [1, 2, 3]
          - polynomialFromList [1, 2]
    )

  assertEqual
    "polynomial multiplication"
    [3, 10, 8]
    ( polynomialCoefficients $
        polynomialFromList [1, 2]
          * polynomialFromList [3, 4]
    )

  assertEqual
    "polynomial power"
    [1, 3, 3, 1]
    (polynomialCoefficients ((x + 1) ^ (3 :: Int)))

  assertEqual
    "polynomial evaluation"
    17
    (polynomialEvaluate (polynomialFromList [1, 2, 3]) 2)

  assertEqual
    "polynomial degree"
    (Just 2)
    (polynomialDegree (polynomialFromList [1, 2, 3]))

  assertEqual
    "zero polynomial degree"
    Nothing
    (polynomialDegree polynomialZero)

  assertEqual
    "polynomial show"
    "1 + 2x + 3x^2"
    (show (polynomialFromList [1, 2, 3]))

  assertEqual
    "polynomial constant term"
    1
    (polynomialConstantTerm (polynomialFromList [1, 2, 3]))

  assertEqual
    "zero polynomial constant term"
    0
    (polynomialConstantTerm polynomialZero)

testPolynomialConversion :: TestTree
testPolynomialConversion =
  testCase "convert polynomial to generating function" $
    assertEqual
      "convert polynomial to generating function"
      [1, 2, 3, 0, 0, 0]
      ( gfTake 6 $
          polynomialToGF (polynomialFromList [1, 2, 3])
      )

testRationalGFConstruction :: TestTree
testRationalGFConstruction = testCase "rational GF construction" $ do
  let numerator = polynomialFromList [1, 1]
      denominator = polynomialFromList [1, -1]

  case rationalGF numerator denominator of
    Left err -> assertFailure ("valid rational generating function returned " ++ show err)
    Right result -> do
      assertEqual
        "rational GF numerator"
        numerator
        (rationalGFNumerator result)

      assertEqual
        "rational GF denominator"
        denominator
        (rationalGFDenominator result)

      assertEqual
        "rational GF display"
        "(1 + x) / (1 - x)"
        (show result)

  case rationalGF polynomialOne polynomialVariable of
    Left DenominatorHasZeroConstantTerm -> pure ()
    Right _ -> assertFailure "denominator with zero constant term was accepted"

testRationalGFConversion :: TestTree
testRationalGFConversion = testCase "rational GF conversion" $ do
  let numerator = polynomialOne
      denominator = polynomialFromList [1, -1]

  case rationalGF numerator denominator of
    Left err -> assertFailure ("rational GF conversion setup returned " ++ show err)
    Right result ->
      assertEqual
        "expand rational GF as a formal power series"
        [1, 1, 1, 1, 1, 1]
        (gfTake 6 (rationalGFToGF result))

  case rationalGF denominator polynomialOne of
    Left err -> assertFailure ("polynomial rational GF returned " ++ show err)
    Right result ->
      assertEqual
        "rational GF display with denominator one"
        "1 - x"
        (show result)

testRecurrenceGeneratingFunction :: TestTree
testRecurrenceGeneratingFunction = testCase "recurrence generating function construction" $ do
  let recurrence = linearRecurrence ((1, 1) :| [(1, 2)])

  assertEqual
    "recurrence order"
    2
    (recurrenceOrder recurrence)

  assertEqual
    "recurrence coefficients"
    [1, 1]
    (recurrenceCoefficients recurrence)

  assertEqual
    "recurrence initial values"
    [1, 2]
    (recurrenceInitialValues recurrence)

  assertEqual
    "recurrence numerator"
    [1, 1]
    (polynomialCoefficients (recurrenceNumerator recurrence))

  assertEqual
    "recurrence denominator"
    [1, -1, -1]
    (polynomialCoefficients (recurrenceDenominator recurrence))

  assertEqual
    "recurrence rational GF"
    "(1 + x) / (1 - x - x^2)"
    (show (recurrenceRationalGF recurrence))

testRecurrenceTerms :: TestTree
testRecurrenceTerms = testCase "recurrence terms" $ do
  let recurrence = linearRecurrence ((1, 1) :| [(1, 1)])

  assertEqual
    "Fibonacci recurrence terms"
    [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    (recurrenceTerms 10 recurrence)

  assertEqual
    "Fibonacci term at index 9"
    (Just 55)
    (recurrenceTermAt 9 recurrence)

  assertEqual
    "negative recurrence term index"
    Nothing
    (recurrenceTermAt (-1) recurrence)

  assertEqual
    "non-positive recurrence term count"
    []
    (recurrenceTerms 0 recurrence)

testThirdOrderRecurrence :: TestTree
testThirdOrderRecurrence = testCase "third-order recurrence" $ do
  let recurrence = linearRecurrence ((1, 0) :| [(1, 0), (1, 1)])

  assertEqual
    "third-order recurrence numerator"
    [0, 0, 1]
    (polynomialCoefficients (recurrenceNumerator recurrence))

  assertEqual
    "third-order recurrence denominator"
    [1, -1, -1, -1]
    (polynomialCoefficients (recurrenceDenominator recurrence))

  assertEqual
    "third-order recurrence terms"
    [0, 0, 1, 1, 2, 4, 7, 13]
    (recurrenceTerms 8 recurrence)

testBuiltins :: TestTree
testBuiltins = testCase "built-in generating functions" $ do
  assertEqual
    "Fibonacci built-in name"
    "fibonacci"
    (builtinName fibonacci)

  assertEqual
    "Fibonacci built-in symbolic form"
    "1 / (1 - x - x^2)"
    (builtinSymbolicForm fibonacci)

  assertEqual
    "Fibonacci built-in coefficients"
    [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    (gfTake 10 (builtinGeneratingFunction fibonacci))

  case lookupBuiltin "FIBONACCI" of
    Nothing -> assertFailure "case-insensitive Fibonacci lookup"
    Just result ->
      assertEqual
        "case-insensitive Fibonacci lookup"
        "fibonacci"
        (builtinName result)

  assertEqual
    "unknown built-in lookup"
    Nothing
    (case lookupBuiltin "unknown" of
       Nothing -> Nothing
       Just result -> Just (builtinName result))

  assertEqual
    "Catalan built-in coefficients"
    [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
    (gfTake 10 (builtinGeneratingFunction catalan))

  assertEqual
    "binaryTrees built-in has the same coefficients as catalan (both satisfy C = 1 + x*C^2)"
    (gfTake 10 (builtinGeneratingFunction catalan))
    (gfTake 10 (builtinGeneratingFunction binaryTrees))

  assertEqual
    "catalan and binaryTrees are distinct entries despite sharing coefficients"
    False
    (builtinName catalan == builtinName binaryTrees)

  assertEqual
    "ternaryTrees built-in coefficients"
    [1, 1, 3, 12, 55, 273, 1428, 7752, 43263, 246675]
    (gfTake 10 (builtinGeneratingFunction ternaryTrees))

  assertEqual
    "partitions built-in coefficients"
    [1, 1, 2, 3, 5, 7, 11, 15, 22, 30]
    (gfTake 10 (builtinGeneratingFunction partitions))

  assertEqual
    "allBuiltins now has five entries"
    5
    (length allBuiltins)

testGfShift :: TestTree
testGfShift = testCase "gfShift" $ do
  assertEqual
    "gfShift 0 is identity"
    [1, 2, 3, 0, 0]
    (gfTake 5 (gfShift 0 (gfFromList [1, 2, 3])))

  assertEqual
    "gfShift 2 prepends two zeros"
    [0, 0, 1, 2, 3, 0, 0]
    (gfTake 7 (gfShift 2 (gfFromList [1, 2, 3])))

-- The Catalan numbers, defined directly as a self-referential GF with
-- gfShift. This definition terminates and produces correct coefficients lazily, whereas
-- the equivalent definition using "gfMul gfVariable" instead of
-- "gfShift 1" does not terminate
--
testCatalanViaSelfReference :: TestTree
testCatalanViaSelfReference =
  testCase "Catalan numbers via self-referential gfShift definition" $
    assertEqual
      "Catalan numbers via self-referential gfShift definition"
      [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
      (gfTake 10 catalan_)
  where
    catalan_ :: GF
    catalan_ = gfAdd gfOne (gfShift 1 (gfMul catalan_ catalan_))

testGfSqrtWithSeed :: TestTree
testGfSqrtWithSeed = testCase "gfSqrtWithSeed" $ do
  case gfSqrtWithSeed 1 (gfFromList [1, -4]) of
    Left err -> assertFailure ("sqrt(1 - 4x) returned " ++ show err)
    Right root -> do
      assertEqual
        "sqrt(1 - 4x) coefficients"
        [1, -2, -2, -4, -10, -28]
        (gfTake 6 root)

      assertEqual
        "sqrt(1 - 4x) squares back to 1 - 4x"
        (gfTake 8 (gfFromList [1, -4]))
        (gfTake 8 (root * root))

  -- case: sqrt(1) = 1
  case gfSqrtWithSeed 1 gfOne of
    Left err -> assertFailure ("sqrt(1) returned " ++ show err)
    Right root ->
      assertEqual
        "sqrt(1) coefficients"
        [1, 0, 0, 0, 0]
        (gfTake 5 root)

testGfSqrtErrors :: TestTree
testGfSqrtErrors = testCase "gfSqrtWithSeed error cases" $ do
  case gfSqrtWithSeed 0 gfOne of
    Left (InvalidSqrtSeed 0 1) -> pure ()
    other -> assertFailure ("zero seed should be rejected, got " ++ show other)

  case gfSqrtWithSeed 2 gfOne of
    Left (InvalidSqrtSeed 2 1) -> pure ()
    other -> assertFailure ("seed not squaring to the constant term should be rejected, got " ++ show other)

testGeneralizedBinomial :: TestTree
testGeneralizedBinomial = testCase "generalizedBinomial" $ do
  assertEqual
    "(1+x)^3 matches ordinary binomial coefficients"
    [1, 3, 3, 1, 0, 0]
    (gfTake 6 (generalizedBinomial 3))

  -- Cross-check: (1 + u)^(1/2) composed with u = -4x should reproduce the
  -- same sqrt(1 - 4x) series computed independently via gfSqrtWithSeed.
  case gfCompose (generalizedBinomial (1 / 2)) (gfFromList [0, -4]) of
    Left err -> assertFailure ("generalizedBinomial composition returned " ++ show err)
    Right viaBinomial ->
      case gfSqrtWithSeed 1 (gfFromList [1, -4]) of
        Left err -> assertFailure ("sqrt(1 - 4x) returned " ++ show err)
        Right viaNewtonStyle ->
          assertEqual
            "generalizedBinomial and gfSqrtWithSeed agree on sqrt(1 - 4x)"
            (gfTake 8 viaNewtonStyle)
            (gfTake 8 viaBinomial)

testCatalanClosedFormMatchesSelfReference :: TestTree
testCatalanClosedFormMatchesSelfReference =
  testCase "closed-form Catalan matches self-referential Catalan" $
    case gfSqrtWithSeed 1 (gfFromList [1, -4]) of
      Left err -> assertFailure ("sqrt(1 - 4x) returned " ++ show err)
      Right root -> do
        let dropLeadingZero gf = gfFromList (tail (gfTake 200 gf))
            numerator = dropLeadingZero (gfOne - root)
            denominator = dropLeadingZero (gfScale 2 gfVariable)

        case gfDivide numerator denominator of
          Left err -> assertFailure ("Catalan closed-form division returned " ++ show err)
          Right closedFormCatalan -> do
            let selfReferentialCatalan :: GF
                selfReferentialCatalan =
                  gfAdd gfOne (gfShift 1 (gfMul selfReferentialCatalan selfReferentialCatalan))

            assertEqual
              "closed-form Catalan matches self-referential Catalan"
              (gfTake 10 selfReferentialCatalan)
              (gfTake 10 closedFormCatalan)

-- Check that a closed form agrees with 'recurrenceTermAt' for every n from
-- 0 up to (and including) maxN, one at a time.
--
-- This is a plain recursive loop, the same shape as e.g. 'divisorsOf' or
-- 'findFirstRoot' elsewhere in the project: the first guard is the base
-- case (stop once n has gone past maxN), the second guard is the
-- recursive case (check the current n, then move on to n + 1).
checkClosedFormAgreesWithRecurrence :: String -> LinearRecurrence -> ClosedFormResult -> Int -> Int -> IO ()
checkClosedFormAgreesWithRecurrence label recurrence closedForm n maxN
  | n > maxN = pure ()
  | otherwise = do
      assertEqual
        (label ++ " matches recurrenceTermAt at n=" ++ show n)
        (recurrenceTermAt n recurrence)
        (closedFormValueAt closedForm n)
      checkClosedFormAgreesWithRecurrence label recurrence closedForm (n + 1) maxN

-- Fibonacci has an irrational (golden-ratio) characteristic root pair, so
-- this is the main test that the quadratic-surd path is correct: the
-- closed form is checked against 'recurrenceTermAt' for many n, and its
-- exact printed form is checked against the hand-derived value
-- ( phi/sqrt(5) and -psi/sqrt(5), written out as (5 +- sqrt(5))/10 ).
testRecurrenceClosedFormFibonacci :: TestTree
testRecurrenceClosedFormFibonacci = testCase "Fibonacci closed form" $ do
  let recurrence = linearRecurrence ((1, 1) :| [(1, 1)])
      closedForm = recurrenceClosedForm recurrence

  case closedForm of
    NoClosedForm reason -> assertFailure ("expected a closed form for Fibonacci, got: " ++ reason)
    ClosedForm _ terms ->
      assertEqual "Fibonacci closed form has two terms" 2 (length terms)

  checkClosedFormAgreesWithRecurrence "Fibonacci closed form" recurrence closedForm 0 20

  assertEqual
    "Fibonacci closed form display"
    "a(n) = (1/2 + 1/10*sqrt(5)) * (1/2 + 1/2*sqrt(5))^n + (1/2 - 1/10*sqrt(5)) * (1/2 - 1/2*sqrt(5))^n"
    (showClosedForm closedForm)

-- a_n = 6 a_(n-1) - 11 a_(n-2) + 6 a_(n-3), with a_n = 1^n + 2^n + 3^n,
-- whose characteristic polynomial y^3 - 6y^2 + 11y - 6 factors completely
-- into three rational roots (1, 2, 3) -- exercises the "no quadratic
-- factor needed at all" path, and a recurrence of order higher than 2.
testRecurrenceClosedFormAllRationalRoots :: TestTree
testRecurrenceClosedFormAllRationalRoots = testCase "order-3 all-rational-root closed form" $ do
  let recurrence = linearRecurrence ((6, 3) :| [(-11, 6), (6, 14)])
      closedForm = recurrenceClosedForm recurrence

  case closedForm of
    NoClosedForm reason -> assertFailure ("expected a closed form, got: " ++ reason)
    ClosedForm _ terms ->
      assertEqual "order-3 closed form has three terms" 3 (length terms)

  checkClosedFormAgreesWithRecurrence "order-3 closed form" recurrence closedForm 0 15

-- a_n = -a_(n-2) has characteristic roots +-i (complex), so no real closed
-- form exists in this system; it must be reported as such, not crash or
-- silently return a wrong answer.
testRecurrenceClosedFormComplexRootsUnavailable :: TestTree
testRecurrenceClosedFormComplexRootsUnavailable =
  testCase "no closed form for a complex-root recurrence" $
    case recurrenceClosedForm recurrence of
      NoClosedForm _ -> pure ()
      ClosedForm _ terms ->
        assertFailure ("expected no closed form for a complex-root recurrence, got: " ++ show terms)
  where
    recurrence = linearRecurrence ((0, 1) :| [(-1, 0)])

-- a_n = 4 a_(n-1) - 4 a_(n-2) has a repeated characteristic root (2, with
-- multiplicity 2), which needs an n * 2^n term this module doesn't
-- produce, so it must be reported as unavailable rather than silently
-- wrong.
testRecurrenceClosedFormRepeatedRootUnavailable :: TestTree
testRecurrenceClosedFormRepeatedRootUnavailable =
  testCase "no closed form for a repeated-root recurrence" $
    case recurrenceClosedForm recurrence of
      NoClosedForm _ -> pure ()
      ClosedForm _ terms ->
        assertFailure ("expected no closed form for a repeated-root recurrence, got: " ++ show terms)
  where
    recurrence = linearRecurrence ((4, 1) :| [(-4, 4)])

-- Catalan numbers via the general guarded solver, driven by a parsed-style
-- Expr rather than hand-written Haskell self-reference -- this is the same
-- equation as 'testCatalanViaSelfReference' above, so it's also a check
-- that 'solveEquation' agrees with the hand-written definition.
testSolveEquationCatalan :: TestTree
testSolveEquationCatalan =
  testCase "Catalan numbers via solveEquation (Y = 1 + x*Y^2)" $
    assertEqual
      "Catalan numbers via solveEquation (Y = 1 + x*Y^2)"
      [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
      (gfTake 10 (solveEquation catalanEquation))

-- The equation for ternary trees, Y = 1 + x*Y^3: a cubic equation, which
-- 'solveEquation' should handle exactly as readily as Catalan's quadratic
-- one, since the guarded self-reference technique doesn't care about the
-- degree of phi.
testSolveEquationTernaryTrees :: TestTree
testSolveEquationTernaryTrees =
  testCase "ternary trees via solveEquation (Y = 1 + x*Y^3)" $
    assertEqual
      "ternary trees via solveEquation (Y = 1 + x*Y^3)"
      [1, 1, 3, 12, 55, 273, 1428, 7752, 43263, 246675]
      (gfTake 10 (solveEquation ternaryTreesEquation))

catalanEquation :: Expr
catalanEquation = Add (Lit 1) (Mul X (Pow Y 2))

ternaryTreesEquation :: Expr
ternaryTreesEquation = Add (Lit 1) (Mul X (Pow Y 3))

-- Lagrange inversion should recognise Catalan's equation as being of the
-- form Y = c + x*phi(Y), and its coefficients should agree exactly with
-- the guarded solver's (two independent algorithms, same answer).
testLagrangeInversionMatchesCatalan :: TestTree
testLagrangeInversionMatchesCatalan =
  testCase "Catalan numbers via Lagrange inversion" $
    case asLagrangeForm catalanEquation of
      Nothing -> assertFailure "expected Catalan's equation to be recognised as Lagrange-invertible"
      Just (c, phi) ->
        assertEqual
          "Catalan numbers via Lagrange inversion"
          [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
          (lagrangeCoefficients c phi 10)

-- Same cross-check for ternary trees: Lagrange inversion handles phi of
-- any degree just as well as the guarded solver does.
testLagrangeInversionMatchesTernaryTrees :: TestTree
testLagrangeInversionMatchesTernaryTrees =
  testCase "ternary trees via Lagrange inversion" $
    case asLagrangeForm ternaryTreesEquation of
      Nothing -> assertFailure "expected ternary trees' equation to be recognised as Lagrange-invertible"
      Just (c, phi) ->
        assertEqual
          "ternary trees via Lagrange inversion"
          [1, 1, 3, 12, 55, 273, 1428, 7752, 43263, 246675]
          (lagrangeCoefficients c phi 10)

-- A custom equation with no additive constant (c = 0) and a mixed
-- quadratic-and-cubic phi, Y = x*(1 + Y^2 + Y^3), checked both ways:
-- against the guarded solver directly, and via Lagrange inversion.
testLagrangeInversionCustomCubic :: TestTree
testLagrangeInversionCustomCubic = testCase "Y = x*(1 + Y^2 + Y^3)" $ do
  let equation = Mul X (Add (Add (Lit 1) (Pow Y 2)) (Pow Y 3))
      expected = [0, 1, 0, 1, 1, 2, 5, 8, 21, 42]

  assertEqual
    "Y = x*(1 + Y^2 + Y^3) via solveEquation"
    expected
    (gfTake 10 (solveEquation equation))

  case asLagrangeForm equation of
    Nothing -> assertFailure "expected Y = x*(1 + Y^2 + Y^3) to be recognised as Lagrange-invertible"
    Just (c, phi) ->
      assertEqual
        "Y = x*(1 + Y^2 + Y^3) via Lagrange inversion"
        expected
        (lagrangeCoefficients c phi 10)

-- Y = 1 + x*Y + x*Y^2 has two separate x*(...) terms, added together
-- rather than already combined into one x*(...) node. 'asLagrangeForm'
-- must notice that x can still be factored out of both of them together
-- (x*Y + x*Y^2 = x*(Y + Y^2)), giving c = 1, phi = Y + Y^2 -- not just
-- recognise equations that already have a single x*(...) term written
-- out. The resulting coefficients are checked against 'solveEquation',
-- which has no such shape restriction and computes them independently.
testLagrangeInversionMatchesMixedEquation :: TestTree
testLagrangeInversionMatchesMixedEquation = testCase "Y = 1 + x*Y + x*Y^2" $ do
  let equation = Add (Add (Lit 1) (Mul X Y)) (Mul X (Pow Y 2))
      expected = [1, 2, 6, 22, 90, 394, 1806, 8558, 41586, 206098]

  assertEqual
    "Y = 1 + x*Y + x*Y^2 via solveEquation"
    expected
    (gfTake 10 (solveEquation equation))

  case asLagrangeForm equation of
    Nothing ->
      assertFailure
        "expected Y = 1 + x*Y + x*Y^2 to be recognised as Lagrange-invertible (x can be factored out of both x*(...) terms together)"
    Just (c, phi) ->
      assertEqual
        "Y = 1 + x*Y + x*Y^2 via Lagrange inversion"
        expected
        (lagrangeCoefficients c phi 10)

-- Y = 1 + x^2*Y has x to the *second* power in its only non-constant
-- term, not the first, so it is genuinely not of the Y = c + x*phi(Y)
-- shape (there is no way to factor out a single x and leave phi free of
-- x). 'asLagrangeForm' must still refuse this, while 'solveEquation'
-- computes its coefficients [1, 0, 1, 0, ...] (= 1/(1-x^2)) regardless.
testAsLagrangeFormRefusesHigherXPower :: TestTree
testAsLagrangeFormRefusesHigherXPower = testCase "Y = 1 + x^2*Y" $ do
  let equation = Add (Lit 1) (Mul (Pow X 2) Y)

  case asLagrangeForm equation of
    Nothing -> pure ()
    Just result ->
      assertFailure ("expected Y = 1 + x^2*Y to be refused by asLagrangeForm, got: " ++ show result)

  assertEqual
    "Y = 1 + x^2*Y via solveEquation"
    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0]
    (gfTake 10 (solveEquation equation))

-- Catalan's equation is quadratic in Y (Y = 1 + x*Y^2), so this checks
-- 'algebraicClosedForm' against both the known Catalan sequence and
-- 'solveEquation' -- two independent algorithms (quadratic formula vs.
-- guarded self-reference) agreeing is a strong correctness check.
testAlgebraicClosedFormCatalan :: TestTree
testAlgebraicClosedFormCatalan = testCase "Catalan via algebraicClosedForm" $
  case algebraicClosedForm catalanEquation 1 of
    Left err -> assertFailure ("Catalan via algebraicClosedForm returned " ++ err)
    Right closedFormCatalan -> do
      assertEqual
        "Catalan via algebraicClosedForm"
        [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
        (gfTake 10 closedFormCatalan)

      assertEqual
        "Catalan via algebraicClosedForm matches solveEquation"
        (gfTake 15 (solveEquation catalanEquation))
        (gfTake 15 closedFormCatalan)

-- Y = x - x*Y + Y^2 (equivalently Y^2 - (1+x)*Y + x = 0) has two
-- perfectly good rational-function roots, Y = 1 and Y = x, with a
-- denominator (2*a(x) = 2) that never vanishes -- so, unlike Catalan,
-- there is no removable singularity forcing one branch; the caller's
-- expected Y(0) genuinely determines which of the two roots comes back.
testAlgebraicClosedFormBranchSelection :: TestTree
testAlgebraicClosedFormBranchSelection = testCase "algebraicClosedForm branch selection" $ do
  let equation = Add (Sub X (Mul X Y)) (Pow Y 2)

  case algebraicClosedForm equation 1 of
    Left err -> assertFailure ("expected the Y(0)=1 branch to succeed, got " ++ err)
    Right root ->
      assertEqual "Y(0) = 1 branch is the constant series 1" [1, 0, 0, 0, 0] (gfTake 5 root)

  case algebraicClosedForm equation 0 of
    Left err -> assertFailure ("expected the Y(0)=0 branch to succeed, got " ++ err)
    Right root ->
      assertEqual "Y(0) = 0 branch is the series x" [0, 1, 0, 0, 0] (gfTake 5 root)

-- A Y(0) that doesn't actually satisfy the equation at x = 0 (Catalan
-- numbers do not start at 2) must be rejected outright, not silently
-- produce a series that doesn't match the recurrence.
testAlgebraicClosedFormRejectsWrongY0 :: TestTree
testAlgebraicClosedFormRejectsWrongY0 =
  testCase "algebraicClosedForm rejects a wrong Y(0)" $
    case algebraicClosedForm catalanEquation 2 of
      Left _ -> pure ()
      Right result ->
        assertFailure ("expected Y(0)=2 to be rejected for Catalan's equation, got: " ++ show (gfTake 5 result))

-- Ternary trees' equation (Y = 1 + x*Y^3) has no Y^2 term at all, so it
-- is not of the quadratic shape and 'asQuadraticInY' must refuse it.
testAsQuadraticInYRefusesNonQuadratic :: TestTree
testAsQuadraticInYRefusesNonQuadratic =
  testCase "asQuadraticInY refuses a non-quadratic equation" $
    case asQuadraticInY ternaryTreesEquation of
      Nothing -> pure ()
      Just result ->
        assertFailure ("expected ternary trees' equation to be refused by asQuadraticInY, got: " ++ show result)

----------------------------------------
-- Property-based tests (QuickCheck)
----------------------------------------
--
-- The tests above are all example-based: given this specific input,
-- expect exactly this specific output. These are property-based instead,
-- general statements (e.g. "addition is commutative") that should hold
-- for *any* valid input, checked against many randomly-generated inputs.

genRational :: Gen Rational
genRational = do
  n <- arbitrary :: Gen Integer
  d <- (arbitrary :: Gen Integer) `suchThat` (/= 0)
  pure (n % d)

instance Arbitrary GF where
  arbitrary = do
    coefficients <- listOf genRational
    pure (gfFromList coefficients)

instance Arbitrary Polynomial where
  arbitrary = do
    coefficients <- listOf genRational
    pure (polynomialFromList coefficients)

-- Compare only the first 20 coefficients, since GF has no (and can't
-- have a well-defined) Eq instance of its own -- it wraps an infinite
-- list, which can't be compared for equality in finite time.
prop_gfAddCommutative :: GF -> GF -> Property
prop_gfAddCommutative a b = gfTake 20 (gfAdd a b) === gfTake 20 (gfAdd b a)

testGfAddCommutative :: TestTree
testGfAddCommutative = testProperty "gfAdd is commutative" prop_gfAddCommutative

prop_gfAddAssociative :: GF -> GF -> GF -> Property
prop_gfAddAssociative a b c =
  gfTake 20 (gfAdd (gfAdd a b) c) === gfTake 20 (gfAdd a (gfAdd b c))

testGfAddAssociative :: TestTree
testGfAddAssociative = testProperty "gfAdd is associative" prop_gfAddAssociative

prop_gfMulCommutative :: GF -> GF -> Property
prop_gfMulCommutative a b = gfTake 20 (gfMul a b) === gfTake 20 (gfMul b a)

testGfMulCommutative :: TestTree
testGfMulCommutative = testProperty "gfMul is commutative" prop_gfMulCommutative

prop_gfMulIdentity :: GF -> Property
prop_gfMulIdentity a = gfTake 20 (gfMul gfOne a) === gfTake 20 a

testGfMulIdentity :: TestTree
testGfMulIdentity = testProperty "gfOne is an identity for gfMul" prop_gfMulIdentity

prop_gfAddIdentity :: GF -> Property
prop_gfAddIdentity a = gfTake 20 (gfAdd gfZero a) === gfTake 20 a

testGfAddIdentity :: TestTree
testGfAddIdentity = testProperty "gfZero is an identity for gfAdd" prop_gfAddIdentity

-- Integrating a series' derivative recovers the series with its constant
-- term zeroed out (integration always reintroduces a constant term of 0,
-- so the original constant term -- whatever it was -- can't survive the
-- round trip).
prop_gfIntegralDerivative :: GF -> Property
prop_gfIntegralDerivative a =
  gfTake 20 (gfIntegral (gfDerivative a))
    === gfTake 20 (gfSub a (gfConstant (gfConstantTerm a)))

testGfIntegralDerivative :: TestTree
testGfIntegralDerivative =
  testProperty "gfIntegral . gfDerivative recovers the non-constant part" prop_gfIntegralDerivative

prop_polynomialAddCommutative :: Polynomial -> Polynomial -> Property
prop_polynomialAddCommutative p q = polynomialAdd p q === polynomialAdd q p

testPolynomialAddCommutative :: TestTree
testPolynomialAddCommutative = testProperty "polynomialAdd is commutative" prop_polynomialAddCommutative

prop_polynomialMulCommutative :: Polynomial -> Polynomial -> Property
prop_polynomialMulCommutative p q = polynomialMul p q === polynomialMul q p

testPolynomialMulCommutative :: TestTree
testPolynomialMulCommutative = testProperty "polynomialMul is commutative" prop_polynomialMulCommutative

prop_polynomialMulAssociative :: Polynomial -> Polynomial -> Polynomial -> Property
prop_polynomialMulAssociative p q r =
  polynomialMul (polynomialMul p q) r === polynomialMul p (polynomialMul q r)

testPolynomialMulAssociative :: TestTree
testPolynomialMulAssociative = testProperty "polynomialMul is associative" prop_polynomialMulAssociative

prop_polynomialEvaluateDistributesOverAdd :: Polynomial -> Polynomial -> Property
prop_polynomialEvaluateDistributesOverAdd p q =
  forAll genRational $ \x ->
    polynomialEvaluate (polynomialAdd p q) x === polynomialEvaluate p x + polynomialEvaluate q x

testPolynomialEvaluateDistributesOverAdd :: TestTree
testPolynomialEvaluateDistributesOverAdd =
  testProperty
    "polynomialEvaluate distributes over polynomialAdd"
    prop_polynomialEvaluateDistributesOverAdd

-- For any order-1 recurrence a_n = c*a_(n-1), a_0 = v
-- (including c = 0), the closed form should match recurrenceTermAt.

prop_order1ClosedFormMatchesTerm :: Rational -> Rational -> NonNegative Int -> Property
prop_order1ClosedFormMatchesTerm c v (NonNegative n) =
    let recurrence = linearRecurrence ((c, v) :| [])
        closedForm = recurrenceClosedForm recurrence
    in  case closedFormValueAt closedForm n of
          Nothing -> property True
          Just value -> Just value === recurrenceTermAt n recurrence

testOrder1ClosedFormMatchesTerm :: TestTree
testOrder1ClosedFormMatchesTerm =
  testProperty
    "order-1 recurrence closed form matches recurrenceTermAt"
    prop_order1ClosedFormMatchesTerm