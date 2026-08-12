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
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)
import Data.Either (isLeft)
import Data.List (isInfixOf, sort)
import GFComb.REPL.Command (Command (..), SeriesExpr (..))
import GFComb.REPL.Eval
  ( Definition (..),
    Env,
    Response (..),
    emptyEnv,
    envLookup,
    envNames,
    evalCommand,
    evalSeriesExpr,
    initialEnv
  )
import GFComb.REPL.Parser
  ( parseCommand,
    parseEquationRhs,
    parseRecurrenceBody,
    parseSeriesExpr
  )
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    NonNegative (..),
    Property,
    choose,
    forAll,
    listOf,
    oneof,
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
            testRecurrenceClosedFormRepeatedRoot
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
          "REPL parser"
          [ testParseQueryExpressions,
            testParseCommands,
            testParseRecurrences,
            testParseRejections
          ],
        testGroup
          "REPL evaluator"
          [ testEvalSeriesExpressions,
            testEvalDefineByRecurrence,
            testEvalDefineByEquation,
            testEvalDefineCubicEquation,
            testEvalRejectsUnguardedEquation,
            testEvalDefinitionsAccumulate,
            testEvalControlCommands,
            testEvalEnvironment
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
            testOrder1ClosedFormMatchesTerm,
            testShowExprRoundTrips
          ],
        testGroup
          "Inhomogeneous recurrences"
          [ testForcedRecurrenceConstant,
            testForcedRecurrenceLinear,
            testForcedRecurrenceTriangular,
            testForcedRecurrenceQuadratic,
            testForcedRecurrenceZeroForcing,
            testForcedRecurrenceTrailingZero,
            testParseForcedRecurrences,
            testEvalDefineForcedRecurrence
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

-- a_n = 4 a_(n-1) - 4 a_(n-2) has the repeated characteristic root 2, so
-- its closed form needs an n * 2^n term. With a(0) = 1 and a(1) = 4 the
-- answer is (1 + n) * 2^n.
testRecurrenceClosedFormRepeatedRoot :: TestTree
testRecurrenceClosedFormRepeatedRoot = testCase "repeated-root closed form" $ do
  let recurrence = linearRecurrence ((4, 1) :| [(-4, 4)])
      closedForm = recurrenceClosedForm recurrence

  case closedForm of
    NoClosedForm reason -> assertFailure ("expected a closed form, got: " ++ reason)
    ClosedForm _ terms ->
      assertEqual "repeated-root closed form has two terms" 2 (length terms)

  checkClosedFormAgreesWithRecurrence "repeated-root closed form" recurrence closedForm 0 15

  assertEqual
    "repeated-root closed form display"
    "a(n) = (1) * (2)^n + (1) * n * (2)^n"
    (showClosedForm closedForm)

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




----------------------------------------
-- REPL: parser
----------------------------------------

-- Assert that something was rejected, and that the message explains why.
--
-- The whole message is not pinned down, only that the informative part of
-- it appears: megaparsec surrounds it with position and caret lines that
-- would make an exact comparison brittle without testing anything useful.
assertRejected :: Show a => String -> String -> Either String a -> IO ()
assertRejected label expectedFragment result =
  case result of
    Right value ->
      assertFailure (label ++ ": expected a rejection, but got " ++ show value)
    Left problem ->
      assertBool
        ( label
            ++ ": the message should mention "
            ++ show expectedFragment
            ++ ", but it was:\n"
            ++ problem
        )
        (expectedFragment `isInfixOf` problem)

testParseQueryExpressions :: TestTree
testParseQueryExpressions = testCase "query expressions" $ do
  -- Without the notFollowedBy guard in the parser, this would come back as
  -- the variable x next to a stray name.
  assertEqual
    "a name beginning with x is one name"
    (Right (SeriesName "xs"))
    (parseSeriesExpr "xs")

  assertEqual
    "a rational literal"
    (Right (SeriesLit (1 / 2)))
    (parseSeriesExpr "1/2")

  -- The same slash means two different things depending on what follows
  -- it, which is why the literal parser has to backtrack.
  assertEqual
    "a slash after a name is division, not part of a literal"
    (Right (SeriesDiv (SeriesName "catalan") (SeriesLit 2)))
    (parseSeriesExpr "catalan/2")

  assertEqual
    "multiplication binds tighter than addition"
    (Right (SeriesAdd (SeriesLit 1) (SeriesMul (SeriesLit 2) SeriesX)))
    (parseSeriesExpr "1 + 2*x")

  assertEqual
    "addition is left associative"
    (Right (SeriesAdd (SeriesAdd (SeriesName "a") (SeriesName "b")) (SeriesName "c")))
    (parseSeriesExpr "a + b + c")

  assertEqual
    "a power binds tighter than multiplication"
    (Right (SeriesMul SeriesX (SeriesPow (SeriesName "c") 2)))
    (parseSeriesExpr "x*c^2")

  assertEqual
    "parentheses override precedence"
    (Right (SeriesMul (SeriesAdd (SeriesLit 1) SeriesX) SeriesX))
    (parseSeriesExpr "(1 + x)*x")

testParseCommands :: TestTree
testParseCommands = testCase "commands" $ do
  assertEqual "help" (Right Help) (parseCommand "help")
  assertEqual "list" (Right ListNames) (parseCommand "list")
  assertEqual "quit" (Right Quit) (parseCommand "quit")
  assertEqual "exit means the same as quit" (Right Quit) (parseCommand "exit")
  assertEqual "show" (Right (ShowName "fib")) (parseCommand "show fib")
  assertEqual "load" (Right (Load "examples.gfcomb")) (parseCommand "load examples.gfcomb")

  -- The count can follow the expression only because multiplication must
  -- be written explicitly: a bare number can never continue an expression,
  -- so the expression parser stops of its own accord.
  assertEqual
    "coeffs takes its count after the expression"
    (Right (Coeffs (SeriesName "fib") 10))
    (parseCommand "coeffs fib 10")

  assertEqual
    "coeffs works with a compound expression"
    (Right (Coeffs (SeriesAdd (SeriesName "fib") (SeriesName "catalan")) 5))
    (parseCommand "coeffs fib + catalan 5")

  -- 'coeff' must not match the start of 'coeffs'.
  assertEqual
    "coeff is not confused with coeffs"
    (Right (CoeffAt (SeriesName "fib") 20))
    (parseCommand "coeff fib 20")

  assertEqual
    "add is sugar for the first ten coefficients of a sum"
    (Right (Coeffs (SeriesAdd (SeriesName "fib") (SeriesName "catalan")) 10))
    (parseCommand "add fib catalan")

  -- Note the Y: the name being defined is resolved to the unknown as it is
  -- parsed, so the Expr can go straight to solveEquation.
  assertEqual
    "an equation definition resolves the defined name to the unknown"
    (Right (DefineByEquation "C" (Add (Lit 1) (Mul X (Pow Y 2)))))
    (parseCommand "define C as solution of: C = 1 + x*C^2")

-- Check that a recurrence body parses to the expected coefficients and
-- initial values.
assertRecurrenceParses :: String -> String -> [Rational] -> [Rational] -> IO ()
assertRecurrenceParses label input expectedCoefficients expectedInitialValues =
  case parseRecurrenceBody input of
    Left problem -> assertFailure (label ++ ": " ++ problem)
    Right recurrence -> do
      assertEqual
        (label ++ " (coefficients)")
        expectedCoefficients
        (recurrenceCoefficients recurrence)
      assertEqual
        (label ++ " (initial values)")
        expectedInitialValues
        (recurrenceInitialValues recurrence)

testParseRecurrences :: TestTree
testParseRecurrences = testCase "recurrence definitions" $ do
  assertRecurrenceParses
    "Fibonacci"
    "a(n) = a(n-1) + a(n-2), a(0)=1, a(1)=1"
    [1, 1]
    [1, 1]

  -- A missing offset is not an error: it is a zero coefficient, and the
  -- order is the largest offset mentioned.
  assertRecurrenceParses
    "a gap becomes a zero coefficient"
    "a(n) = a(n-1) + a(n-3), a(0)=1, a(1)=1, a(2)=1"
    [1, 0, 1]
    [1, 1, 1]

  -- This is the shape that used to produce a silently wrong closed form,
  -- before normalizeRecurrence: a coefficient list ending in zero.
  assertRecurrenceParses
    "an explicit zero coefficient gives a trailing zero"
    "a(n) = a(n-1) + 0*a(n-2), a(0)=1, a(1)=1"
    [1, 0]
    [1, 1]

  assertRecurrenceParses
    "coefficients and subtraction"
    "a(n) = 3*a(n-1) - 2*a(n-2), a(0)=1, a(1)=3"
    [3, -2]
    [1, 3]

  assertRecurrenceParses
    "repeated offsets are summed"
    "a(n) = a(n-1) + a(n-1), a(0)=1"
    [2]
    [1]

  assertRecurrenceParses
    "the name of the sequence is the user's to choose"
    "fib(n) = fib(n-1) + fib(n-2), fib(0)=1, fib(1)=1"
    [1, 1]
    [1, 1]

  assertRecurrenceParses
    "initial values may be negative or fractional"
    "a(n) = a(n-1), a(0) = -1/2"
    [1]
    [- (1 / 2)]

testParseRejections :: TestTree
testParseRejections = testCase "rejected input" $ do
  assertRejected
    "too few initial values"
    "this recurrence has order 2"
    (parseRecurrenceBody "a(n) = a(n-1) + a(n-2), a(0)=1")

  assertRejected
    "too many initial values"
    "remove"
    (parseRecurrenceBody "a(n) = a(n-1), a(0)=1, a(1)=1")

  assertRejected
    "an initial value given twice"
    "given more than once"
    (parseRecurrenceBody "a(n) = a(n-1) + a(n-2), a(0)=1, a(0)=2")

  assertRejected
    "an offset of zero would be circular"
    "circular"
    (parseRecurrenceBody "a(n) = a(n-0), a(0)=1")

  assertRejected
    "a definition may not mention another name"
    "unknown name 'C'"
    (parseEquationRhs "T" "1 + x*C^2")

  assertBool
    "a command keyword cannot be used as a name"
    (isLeft (parseCommand "define list by recurrence: a(n) = a(n-1), a(0)=1"))

  assertBool
    "coeffs needs a count"
    (isLeft (parseCommand "coeffs fib"))

  assertBool
    "implicit multiplication is not accepted"
    (isLeft (parseEquationRhs "T" "2x"))

----------------------------------------
-- REPL: evaluator
----------------------------------------

-- Parse and run one line of REPL input, as the loop would.
--
-- Fails the test if the line does not parse, or if it asks the loop to do
-- something rather than producing output.
runReplLine :: Env -> String -> IO ([String], Env)
runReplLine env line =
  case parseCommand line of
    Left problem -> assertFailure ("could not parse " ++ show line ++ ":\n" ++ problem)
    Right parsedCommand ->
      case evalCommand env parsedCommand of
        (Output outputLines, nextEnv) -> pure (outputLines, nextEnv)
        (other, _) ->
          assertFailure ("expected output from " ++ show line ++ ", but got " ++ show other)

-- Assert that some line of output contains the given text.
assertMentioned :: String -> String -> [String] -> IO ()
assertMentioned label expectedFragment outputLines =
  assertBool
    (label ++ ": expected " ++ show expectedFragment ++ " somewhere in:\n" ++ unlines outputLines)
    (any (expectedFragment `isInfixOf`) outputLines)

testEvalSeriesExpressions :: TestTree
testEvalSeriesExpressions = testCase "evaluating query expressions" $ do
  assertEqual
    "a built-in resolves to its series"
    (Right [1, 1, 2, 5, 14])
    (fmap (gfTake 5) (evalSeriesExpr initialEnv (SeriesName "catalan")))

  assertEqual
    "1/(1 - x) is the all-ones series"
    (Right [1, 1, 1, 1, 1])
    ( fmap
        (gfTake 5)
        (evalSeriesExpr initialEnv (SeriesDiv (SeriesLit 1) (SeriesSub (SeriesLit 1) SeriesX)))
    )

  assertEqual
    "two built-ins can be combined"
    (Right (zipWith (+) catalanTerms fibonacciTerms))
    ( fmap
        (gfTake 5)
        (evalSeriesExpr initialEnv (SeriesAdd (SeriesName "catalan") (SeriesName "fibonacci")))
    )

  assertRejected
    "an undefined name"
    "is not defined"
    (evalSeriesExpr initialEnv (SeriesName "nosuchseries"))

  assertRejected
    "dividing by a series with a zero constant term"
    "constant term is 0"
    (evalSeriesExpr initialEnv (SeriesDiv (SeriesLit 1) SeriesX))
  where
    catalanTerms = gfTake 5 (builtinGeneratingFunction catalan)
    fibonacciTerms = gfTake 5 (builtinGeneratingFunction fibonacci)

testEvalDefineByRecurrence :: TestTree
testEvalDefineByRecurrence = testCase "defining by recurrence" $ do
  (output, env) <-
    runReplLine
      initialEnv
      "define fib by recurrence: a(n) = a(n-1) + a(n-2), a(0)=1, a(1)=1"

  assertMentioned "the rational generating function is shown" "1 / (1 - x - x^2)" output
  assertMentioned "the closed form is shown" "sqrt(5)" output

  case envLookup "fib" env of
    Nothing -> assertFailure "'fib' should be defined afterwards"
    Just definition ->
      assertEqual
        "its coefficients are the Fibonacci numbers"
        [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
        (gfTake 10 (definitionSeries definition))

testEvalDefineByEquation :: TestTree
testEvalDefineByEquation = testCase "defining by equation" $ do
  (output, env) <- runReplLine initialEnv "define C as solution of: C = 1 + x*C^2"

  -- The same string that Builtins.hs carries for catalan, but produced
  -- here from the user's own equation rather than hardcoded.
  assertMentioned "the closed form is rendered symbolically" "(1 - sqrt(1 - 4x)) / (2x)" output

  case envLookup "C" env of
    Nothing -> assertFailure "'C' should be defined afterwards"
    Just definition ->
      assertEqual
        "a user-typed Catalan equation agrees with the built-in"
        (gfTake 12 (builtinGeneratingFunction catalan))
        (gfTake 12 (definitionSeries definition))

testEvalDefineCubicEquation :: TestTree
testEvalDefineCubicEquation = testCase "defining a cubic equation" $ do
  (output, env) <- runReplLine initialEnv "define T as solution of: T = 1 + x*T^3"

  -- No closed form for a cubic, but the coefficients still come out, which
  -- is why they are shown even when the generating function cannot be.
  assertMentioned "the absence of a closed form is explained" "no closed form" output

  case envLookup "T" env of
    Nothing -> assertFailure "'T' should be defined afterwards"
    Just definition ->
      assertEqual
        "its coefficients count ternary trees"
        (gfTake 10 (builtinGeneratingFunction ternaryTrees))
        (gfTake 10 (definitionSeries definition))

testEvalRejectsUnguardedEquation :: TestTree
testEvalRejectsUnguardedEquation = testCase "an unguarded equation is refused" $ do
  -- Note this test hangs rather than fails if isGuardedEquation is broken:
  -- solveEquation does not reject an unguarded equation, it fails to
  -- terminate, and describing the definition would force it.
  (output, env) <- runReplLine initialEnv "define bad as solution of: bad = 1 + bad^2"

  assertMentioned "the refusal explains what is wrong" "must be multiplied by x" output

  assertEqual
    "nothing is defined by a refused definition"
    Nothing
    (fmap definitionName (envLookup "bad" env))

testEvalDefinitionsAccumulate :: TestTree
testEvalDefinitionsAccumulate = testCase "definitions accumulate and can be combined" $ do
  (_, afterFirst) <-
    runReplLine initialEnv "define fib by recurrence: a(n) = a(n-1) + a(n-2), a(0)=1, a(1)=1"
  (_, afterSecond) <- runReplLine afterFirst "define C as solution of: C = 1 + x*C^2"
  (output, _) <- runReplLine afterSecond "coeffs fib + C 5"

  assertEqual
    "the sum of the two is reported"
    ["[2, 2, 4, 8, 19]"]
    output

testEvalControlCommands :: TestTree
testEvalControlCommands = testCase "quit and load are reported, not performed" $ do
  assertEqual
    "quit"
    (Right QuitRequested)
    (fmap (fst . evalCommand initialEnv) (parseCommand "quit"))

  assertEqual
    "load"
    (Right (LoadRequested "examples.gfcomb"))
    (fmap (fst . evalCommand initialEnv) (parseCommand "load examples.gfcomb"))

testEvalEnvironment :: TestTree
testEvalEnvironment = testCase "the session starts with the built-ins defined" $ do
  assertEqual
    "every built-in is present from the start"
    (sort (map builtinName allBuiltins))
    (sort (envNames initialEnv))

  assertEqual "an empty environment has nothing in it" [] (envNames emptyEnv)

  (output, _) <- runReplLine initialEnv "show catalan"
  assertMentioned "show describes a built-in" "(1 - sqrt(1 - 4x)) / (2x)" output

  (missing, _) <- runReplLine initialEnv "show nosuchname"
  assertMentioned "show reports an unknown name" "is not defined" missing

----------------------------------------
-- REPL: parser and printer agree
----------------------------------------

-- Generate an expression of bounded depth.
--
-- Literals are kept non-negative on purpose. The parser reads @-3@ as a
-- prefix minus applied to @3@, i.e. @Sub (Lit 0) (Lit 3)@, so an Expr it
-- produces never contains a negative literal - and the round-trip
-- property below is about expressions the parser could have produced.
genExprOfDepth :: Int -> Gen Expr
genExprOfDepth depth
  | depth <= 0 = oneof atoms
  | otherwise =
      oneof
        ( atoms
            ++ [ Add <$> smaller <*> smaller,
                 Sub <$> smaller <*> smaller,
                 Mul <$> smaller <*> smaller,
                 Pow <$> smaller <*> (fromIntegral <$> choose (0 :: Int, 4))
               ]
        )
  where
    smaller = genExprOfDepth (depth - 1)
    atoms = [pure X, pure Y, Lit . abs <$> genRational]

-- Printing an expression and parsing it back returns the same expression.
--
-- This checks 'showExpr' and the equation parser against each other: a
-- precedence or associativity mistake in either one shows up here, because
-- the two would have to make exactly the same mistake to agree.
prop_showExprRoundTrips :: Property
prop_showExprRoundTrips =
  forAll (genExprOfDepth 3) $ \expression ->
    parseEquationRhs "T" (showExpr "T" expression) === Right expression

testShowExprRoundTrips :: TestTree
testShowExprRoundTrips =
  testProperty "showExpr and the equation parser agree" prop_showExprRoundTrips

----------------------------------------
-- Inhomogeneous recurrences
----------------------------------------

-- a(n) = 2*a(n-1) + 1, a(0) = 1: the Tower of Hanoi, and the simplest
-- recurrence that cannot be written homogeneously at all. Converting it
-- raises the order from 1 to 2, giving characteristic roots 1 and 2 and
-- the closed form 2^(n+1) - 1.
testForcedRecurrenceConstant :: TestTree
testForcedRecurrenceConstant = testCase "constant forcing term (Tower of Hanoi)" $ do
  let recurrence = forcedRecurrence ((2, 1) :| []) (polynomialFromList [1])

  assertEqual
    "the converted recurrence has order 2"
    2
    (recurrenceOrder recurrence)

  assertEqual
    "Tower of Hanoi terms"
    [1, 3, 7, 15, 31, 63, 127, 255, 511, 1023]
    (recurrenceTerms 10 recurrence)

  checkClosedFormAgreesWithRecurrence
    "Tower of Hanoi closed form"
    recurrence
    (recurrenceClosedForm recurrence)
    0
    15

-- a(n) = 2*a(n-1) + n, a(0) = 0. A forcing term of degree 1 raises the
-- order by two and introduces the root 1 twice, so this is also a check
-- that the repeated-root closed form and the conversion work together: the
-- answer, 2^(n+1) - n - 2, could not be expressed without the n^j terms.
testForcedRecurrenceLinear :: TestTree
testForcedRecurrenceLinear = testCase "linear forcing term" $ do
  let recurrence = forcedRecurrence ((2, 0) :| []) (polynomialFromList [0, 1])

  assertEqual
    "the converted recurrence has order 3"
    3
    (recurrenceOrder recurrence)

  assertEqual
    "terms of a(n) = 2a(n-1) + n"
    [0, 1, 4, 11, 26, 57, 120, 247, 502, 1013]
    (recurrenceTerms 10 recurrence)

  case recurrenceClosedForm recurrence of
    NoClosedForm reason -> assertFailure ("expected a closed form, got: " ++ reason)
    ClosedForm _ terms ->
      assertBool
        "the closed form contains a term with a power of n"
        (any ((> 0) . termPower) terms)

  checkClosedFormAgreesWithRecurrence
    "a(n) = 2a(n-1) + n closed form"
    recurrence
    (recurrenceClosedForm recurrence)
    0
    15

-- a(n) = a(n-1) + n, a(0) = 0: the triangular numbers, whose closed form
-- n(n+1)/2 is a polynomial in n with no exponential part at all. The
-- characteristic root is 1 with multiplicity 3.
testForcedRecurrenceTriangular :: TestTree
testForcedRecurrenceTriangular = testCase "triangular numbers" $ do
  let recurrence = forcedRecurrence ((1, 0) :| []) (polynomialFromList [0, 1])

  assertEqual
    "triangular numbers"
    [0, 1, 3, 6, 10, 15, 21, 28, 36, 45]
    (recurrenceTerms 10 recurrence)

  checkClosedFormAgreesWithRecurrence
    "triangular closed form"
    recurrence
    (recurrenceClosedForm recurrence)
    0
    15

-- A quadratic forcing term, on a recurrence of order greater than one, to
-- check that the conversion does not quietly assume either is small.
testForcedRecurrenceQuadratic :: TestTree
testForcedRecurrenceQuadratic = testCase "quadratic forcing term on an order-2 recurrence" $ do
  let recurrence = forcedRecurrence ((1, 0) :| [(1, 1)]) (polynomialFromList [0, 0, 1])

  assertEqual
    "the converted recurrence has order 5"
    5
    (recurrenceOrder recurrence)

  -- a(n) = a(n-1) + a(n-2) + n^2, computed by hand from a(0)=0, a(1)=1:
  -- a(2) = 1 + 0 + 4 = 5, a(3) = 5 + 1 + 9 = 15, a(4) = 15 + 5 + 16 = 36,
  -- a(5) = 36 + 15 + 25 = 76, a(6) = 76 + 36 + 36 = 148.
  assertEqual
    "terms of a(n) = a(n-1) + a(n-2) + n^2"
    [0, 1, 5, 15, 36, 76, 148]
    (recurrenceTerms 7 recurrence)

-- A zero forcing polynomial must leave the recurrence exactly as it was,
-- since every homogeneous recurrence is a forced one with f = 0.
testForcedRecurrenceZeroForcing :: TestTree
testForcedRecurrenceZeroForcing = testCase "a zero forcing term changes nothing" $
  assertEqual
    "forcedRecurrence with a zero forcing polynomial is linearRecurrence"
    (linearRecurrence ((1, 1) :| [(1, 1)]))
    (forcedRecurrence ((1, 1) :| [(1, 1)]) polynomialZero)

-- The base recurrence's own last coefficient is zero here, so the
-- extended denominator normalizes to a shorter coefficient list than the
-- order it stands for. Without padding those zeros back the converted
-- recurrence would be an order short, and would describe a different
-- sequence from the third term onwards.
testForcedRecurrenceTrailingZero :: TestTree
testForcedRecurrenceTrailingZero = testCase "forcing a recurrence with a trailing zero coefficient" $ do
  let recurrence = forcedRecurrence ((3, 1) :| [(0, 7)]) (polynomialFromList [1])

  assertEqual
    "the converted recurrence keeps its full order"
    3
    (recurrenceOrder recurrence)

  -- a(n) = 3*a(n-1) + 0*a(n-2) + 1, from a(0)=1, a(1)=7:
  -- a(2) = 22, a(3) = 67, a(4) = 202.
  assertEqual
    "terms are those of the original inhomogeneous rule"
    [1, 7, 22, 67, 202]
    (recurrenceTerms 5 recurrence)

testParseForcedRecurrences :: TestTree
testParseForcedRecurrences = testCase "recurrences with a forcing term" $ do
  assertRecurrenceTerms
    "a constant forcing term"
    "a(n) = 2*a(n-1) + 1, a(0)=1"
    [1, 3, 7, 15, 31, 63]

  assertRecurrenceTerms
    "a bare n"
    "a(n) = 2*a(n-1) + n, a(0)=0"
    [0, 1, 4, 11, 26, 57]

  assertRecurrenceTerms
    "a coefficient on a power of n"
    "a(n) = a(n-1) + 3*n^2, a(0)=0"
    [0, 3, 15, 42, 90, 165]

  assertRecurrenceTerms
    "a subtracted forcing term"
    "a(n) = a(n-1) - n, a(0)=0"
    [0, -1, -3, -6, -10, -15]

  assertRecurrenceTerms
    "a rational coefficient in the forcing term"
    "a(n) = a(n-1) + 1/2*n, a(0)=0"
    [0, 1 / 2, 3 / 2, 3, 5, 15 / 2]

  assertRecurrenceTerms
    "alpha*a(n-1) + beta"
    "a(n) = 3*a(n-1) + 5, a(0)=2"
    [2, 11, 38, 119, 362, 1091]

  -- The forcing term does not change the order, so this still needs
  -- exactly the two initial values its references to earlier terms imply.
  assertRecurrenceTerms
    "a forcing term alongside an order-2 recurrence"
    "a(n) = a(n-1) + a(n-2) + 1, a(0)=0, a(1)=1"
    [0, 1, 2, 4, 7, 12]

  assertRecurrenceTerms
    "a formula in n with no earlier terms"
    "a(n) = n + 1"
    [1, 2, 3, 4, 5, 6]

  assertRecurrenceTerms
    "a quadratic formula in n"
    "a(n) = n^2"
    [0, 1, 4, 9, 16, 25]

  assertRejected
    "the forcing term does not excuse a missing initial value"
    "this recurrence has order 2"
    (parseRecurrenceBody "a(n) = a(n-1) + a(n-2) + 1, a(0)=0")

-- Parse a recurrence body and check the sequence it describes.
assertRecurrenceTerms :: String -> String -> [Rational] -> IO ()
assertRecurrenceTerms label input expected =
  case parseRecurrenceBody input of
    Left problem -> assertFailure (label ++ ": " ++ problem)
    Right recurrence ->
      assertEqual label expected (recurrenceTerms (length expected) recurrence)

testEvalDefineForcedRecurrence :: TestTree
testEvalDefineForcedRecurrence = testCase "defining an inhomogeneous recurrence" $ do
  (output, env) <- runReplLine initialEnv "define hanoi by recurrence: a(n) = 2*a(n-1) + 1, a(0)=1"

  assertMentioned "a closed form is found" "Closed form:" output

  case envLookup "hanoi" env of
    Nothing -> assertFailure "'hanoi' should be defined afterwards"
    Just definition ->
      assertEqual
        "its coefficients are the Tower of Hanoi numbers"
        [1, 3, 7, 15, 31, 63, 127, 255, 511, 1023]
        (gfTake 10 (definitionSeries definition))