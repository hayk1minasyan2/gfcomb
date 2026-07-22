module Main (main) where

import Control.Monad (unless)
import GFComb.Core
import GFComb.Polynomial
import GFComb.Conversion
import GFComb.RationalGF
import GFComb.Recurrence
import GFComb.AlgebraicGF
import GFComb.Builtins
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "Running GFComb tests..."

  testConstruction
  testAddition
  testMultiplication
  testDerivative
  testIntegral
  testDivision
  testComposition
  testErrors
  testInfiniteMultiplication

  testPolynomial

  testPolynomialConversion

  testRationalGFConstruction

  testRationalGFConversion

  testRecurrenceValidation
  testRecurrenceGeneratingFunction
  testRecurrenceTerms
  testThirdOrderRecurrence

  testBuiltins

  testGfShift
  testCatalanViaSelfReference
  testGfSqrtWithSeed
  testGfSqrtErrors
  testGeneralizedBinomial
  testCatalanClosedFormMatchesSelfReference

  testRecurrenceClosedFormFibonacci
  testRecurrenceClosedFormAllRationalRoots
  testRecurrenceClosedFormComplexRootsUnavailable
  testRecurrenceClosedFormRepeatedRootUnavailable

  testSolveEquationCatalan
  testSolveEquationTernaryTrees
  testLagrangeInversionMatchesCatalan
  testLagrangeInversionMatchesTernaryTrees
  testLagrangeInversionCustomCubic
  testLagrangeInversionMatchesMixedEquation
  testAsLagrangeFormRefusesHigherXPower
  
  testAlgebraicClosedFormCatalan
  testAlgebraicClosedFormBranchSelection
  testAlgebraicClosedFormRejectsWrongY0
  testAsQuadraticInYRefusesNonQuadratic

  putStrLn "All tests passed."

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName expected actual =
  unless (expected == actual) $ do
    putStrLn ("FAILED: " ++ testName)
    putStrLn ("  Expected: " ++ show expected)
    putStrLn ("  Actual:   " ++ show actual)
    exitFailure

testConstruction :: IO ()
testConstruction =
  assertEqual
    "gfFromList pads with zeros"
    [1, 2, 3, 0, 0, 0, 0, 0, 0, 0]
    (gfTake 10 (gfFromList [1, 2, 3]))

testAddition :: IO ()
testAddition =
  assertEqual
    "addition"
    [4, 6, 3, 0, 0]
    ( gfTake 5 $
        gfFromList [1, 2, 3] + gfFromList [3, 4]
    )

testMultiplication :: IO ()
testMultiplication =
  assertEqual
    "multiplication"
    [3, 10, 8, 0, 0]
    ( gfTake 5 $
        gfFromList [1, 2] * gfFromList [3, 4]
    )

testDerivative :: IO ()
testDerivative =
  assertEqual
    "derivative"
    [2, 6, 12, 0, 0]
    (gfTake 5 (gfDerivative (gfFromList [1, 2, 3, 4])))

testIntegral :: IO ()
testIntegral =
  assertEqual
    "integral"
    [0, 2, 3, 4, 0]
    (gfTake 5 (gfIntegral (gfFromList [2, 6, 12])))

testDivision :: IO ()
testDivision = do
  let x = gfVariable

  case gfDivide gfOne (gfOne - x) of
    Left err -> do
      putStrLn ("FAILED: division returned " ++ show err)
      exitFailure

    Right result ->
      assertEqual
        "geometric-series division"
        [1, 1, 1, 1, 1, 1]
        (gfTake 6 result)

testComposition :: IO ()
testComposition = do
  let x = gfVariable
      outer = gfFromList [2, 3, 4]
      inner = x + x * x

  case gfCompose outer inner of
    Left err -> do
      putStrLn ("FAILED: composition returned " ++ show err)
      exitFailure

    Right result ->
      assertEqual
        "composition"
        [2, 3, 7, 8, 4, 0]
        (gfTake 6 result)

testErrors :: IO ()
testErrors = do
  case gfDivide gfOne gfVariable of
    Left DivisionByZeroConstant ->
      pure ()

    Left otherError -> do
      putStrLn "FAILED: division by a series with zero constant term"
      putStrLn "  Expected: DivisionByZeroConstant"
      putStrLn ("  Actual:   " ++ show otherError)
      exitFailure

    Right result -> do
      putStrLn "FAILED: division by a series with zero constant term"
      putStrLn ("  Expected an error, but received: " ++ show result)
      exitFailure

  case gfCompose gfVariable (gfConstant 2) of
    Left (CompositionRequiresZeroConstant constant)
      | constant == 2 ->
          pure ()

    Left otherError -> do
      putStrLn "FAILED: composition requires zero inner constant term"
      putStrLn "  Expected: CompositionRequiresZeroConstant 2"
      putStrLn ("  Actual:   " ++ show otherError)
      exitFailure

    Right result -> do
      putStrLn "FAILED: composition requires zero inner constant term"
      putStrLn ("  Expected an error, but received: " ++ show result)
      exitFailure


testInfiniteMultiplication :: IO ()
testInfiniteMultiplication = do
  let x = gfVariable

  case gfReciprocal (gfOne - x) of
    Left err -> do
      putStrLn ("FAILED: infinite-series multiplication setup returned " ++ show err)
      exitFailure

    Right geometric ->
      assertEqual
        "multiplication of infinite geometric series"
        [1, 2, 3, 4, 5, 6]
        (gfTake 6 (geometric * geometric))


testPolynomial :: IO ()
testPolynomial = do
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


testPolynomialConversion :: IO ()
testPolynomialConversion =
  assertEqual
    "convert polynomial to generating function"
    [1, 2, 3, 0, 0, 0]
    ( gfTake 6 $
        polynomialToGF (polynomialFromList [1, 2, 3])
    )


testRationalGFConstruction :: IO ()
testRationalGFConstruction = do
  let numerator = polynomialFromList [1, 1]
      denominator = polynomialFromList [1, -1]

  case rationalGF numerator denominator of
    Left err -> do
      putStrLn ("FAILED: valid rational generating function returned " ++ show err)
      exitFailure

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
    Left DenominatorHasZeroConstantTerm ->
      pure ()

    Right _ -> do
      putStrLn "FAILED: denominator with zero constant term was accepted"
      exitFailure

testRationalGFConversion :: IO ()
testRationalGFConversion = do
  let numerator = polynomialOne
      denominator = polynomialFromList [1, -1]

  case rationalGF numerator denominator of
    Left err -> do
      putStrLn ("FAILED: rational GF conversion setup returned " ++ show err)
      exitFailure

    Right result ->
      assertEqual
        "expand rational GF as a formal power series"
        [1, 1, 1, 1, 1, 1]
        (gfTake 6 (rationalGFToGF result))

  case rationalGF denominator polynomialOne of
    Left err -> do
      putStrLn ("FAILED: polynomial rational GF returned " ++ show err)
      exitFailure

    Right result ->
      assertEqual
        "rational GF display with denominator one"
        "1 - x"
        (show result)

testRecurrenceValidation :: IO ()
testRecurrenceValidation = do
  case linearRecurrence [] [] of
    Left EmptyRecurrenceCoefficients ->
      pure ()

    Left otherError -> do
      putStrLn "FAILED: empty recurrence coefficients"
      putStrLn ("  Unexpected error: " ++ show otherError)
      exitFailure

    Right _ -> do
      putStrLn "FAILED: empty recurrence coefficients were accepted"
      exitFailure

  case linearRecurrence [1, 1] [1] of
    Left
      InitialValueCountMismatch
        { expectedInitialValueCount = 2,
          actualInitialValueCount = 1} ->
        pure ()

    Left otherError -> do
      putStrLn "FAILED: incorrect initial-value count"
      putStrLn ("  Unexpected error: " ++ show otherError)
      exitFailure

    Right _ -> do
      putStrLn "FAILED: incorrect initial-value count was accepted"
      exitFailure


testRecurrenceGeneratingFunction :: IO ()
testRecurrenceGeneratingFunction =
  case linearRecurrence [1, 1] [1, 2] of
    Left err -> do
      putStrLn ("FAILED: recurrence construction returned " ++ show err)
      exitFailure

    Right recurrence -> do
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

testRecurrenceTerms :: IO ()
testRecurrenceTerms =
  case linearRecurrence [1, 1] [1, 1] of
    Left err -> do
      putStrLn ("FAILED: Fibonacci recurrence returned " ++ show err)
      exitFailure

    Right recurrence -> do
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

testThirdOrderRecurrence :: IO ()
testThirdOrderRecurrence =
  case linearRecurrence [1, 1, 1] [0, 0, 1] of
    Left err -> do
      putStrLn ("FAILED: third-order recurrence returned " ++ show err)
      exitFailure

    Right recurrence -> do
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


testBuiltins :: IO ()
testBuiltins = do
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
    Nothing -> do
      putStrLn "FAILED: case-insensitive Fibonacci lookup"
      exitFailure

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


testGfShift :: IO ()
testGfShift = do
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
testCatalanViaSelfReference :: IO ()
testCatalanViaSelfReference =
  assertEqual
    "Catalan numbers via self-referential gfShift definition"
    [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
    (gfTake 10 catalan_)
  where
    catalan_ :: GF
    catalan_ = gfAdd gfOne (gfShift 1 (gfMul catalan_ catalan_))

testGfSqrtWithSeed :: IO ()
testGfSqrtWithSeed = do
  case gfSqrtWithSeed 1 (gfFromList [1, -4]) of
    Left err -> do
      putStrLn ("FAILED: sqrt(1 - 4x) returned " ++ show err)
      exitFailure

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
    Left err -> do
      putStrLn ("FAILED: sqrt(1) returned " ++ show err)
      exitFailure

    Right root ->
      assertEqual
        "sqrt(1) coefficients"
        [1, 0, 0, 0, 0]
        (gfTake 5 root)

testGfSqrtErrors :: IO ()
testGfSqrtErrors = do
  case gfSqrtWithSeed 0 gfOne of
    Left (InvalidSqrtSeed 0 1) -> pure ()
    other -> do
      putStrLn ("FAILED: zero seed should be rejected, got " ++ show other)
      exitFailure

  case gfSqrtWithSeed 2 gfOne of
    Left (InvalidSqrtSeed 2 1) -> pure ()
    other -> do
      putStrLn ("FAILED: seed not squaring to the constant term should be rejected, got " ++ show other)
      exitFailure

testGeneralizedBinomial :: IO ()
testGeneralizedBinomial = do
  assertEqual
    "(1+x)^3 matches ordinary binomial coefficients"
    [1, 3, 3, 1, 0, 0]
    (gfTake 6 (generalizedBinomial 3))

  -- Cross-check: (1 + u)^(1/2) composed with u = -4x should reproduce the
  -- same sqrt(1 - 4x) series computed independently via gfSqrtWithSeed.
  case gfCompose (generalizedBinomial (1 / 2)) (gfFromList [0, -4]) of
    Left err -> do
      putStrLn ("FAILED: generalizedBinomial composition returned " ++ show err)
      exitFailure

    Right viaBinomial ->
      case gfSqrtWithSeed 1 (gfFromList [1, -4]) of
        Left err -> do
          putStrLn ("FAILED: sqrt(1 - 4x) returned " ++ show err)
          exitFailure

        Right viaNewtonStyle ->
          assertEqual
            "generalizedBinomial and gfSqrtWithSeed agree on sqrt(1 - 4x)"
            (gfTake 8 viaNewtonStyle)
            (gfTake 8 viaBinomial)


testCatalanClosedFormMatchesSelfReference :: IO ()
testCatalanClosedFormMatchesSelfReference =
  case gfSqrtWithSeed 1 (gfFromList [1, -4]) of
    Left err -> do
      putStrLn ("FAILED: sqrt(1 - 4x) returned " ++ show err)
      exitFailure

    Right root -> do
      let dropLeadingZero gf = gfFromList (tail (gfTake 200 gf))
          numerator = dropLeadingZero (gfOne - root)
          denominator = dropLeadingZero (gfScale 2 gfVariable)

      case gfDivide numerator denominator of
        Left err -> do
          putStrLn ("FAILED: Catalan closed-form division returned " ++ show err)
          exitFailure

        Right closedFormCatalan -> do
          let selfReferentialCatalan :: GF
              selfReferentialCatalan =
                gfAdd gfOne (gfShift 1 (gfMul selfReferentialCatalan selfReferentialCatalan))

          assertEqual
            "closed-form Catalan matches self-referential Catalan"
            (gfTake 10 selfReferentialCatalan)
            (gfTake 10 closedFormCatalan)

----------------------------------------
-- Closed form for linear recurrences
----------------------------------------

-- Check that a closed form agrees with 'recurrenceTermAt' for every n from
-- 0 up to (and including) maxN, one at a time.
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
testRecurrenceClosedFormFibonacci :: IO ()
testRecurrenceClosedFormFibonacci =
  case linearRecurrence [1, 1] [1, 1] of
    Left err -> do
      putStrLn ("FAILED: Fibonacci recurrence returned " ++ show err)
      exitFailure

    Right recurrence -> do
      let closedForm = recurrenceClosedForm recurrence

      case closedForm of
        NoClosedForm reason -> do
          putStrLn ("FAILED: expected a closed form for Fibonacci, got: " ++ reason)
          exitFailure
        ClosedForm terms ->
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
testRecurrenceClosedFormAllRationalRoots :: IO ()
testRecurrenceClosedFormAllRationalRoots =
  case linearRecurrence [6, -11, 6] [3, 6, 14] of
    Left err -> do
      putStrLn ("FAILED: order-3 all-rational-root recurrence returned " ++ show err)
      exitFailure

    Right recurrence -> do
      let closedForm = recurrenceClosedForm recurrence

      case closedForm of
        NoClosedForm reason -> do
          putStrLn ("FAILED: expected a closed form, got: " ++ reason)
          exitFailure
        ClosedForm terms ->
          assertEqual "order-3 closed form has three terms" 3 (length terms)

      checkClosedFormAgreesWithRecurrence "order-3 closed form" recurrence closedForm 0 15

-- a_n = -a_(n-2) has characteristic roots +-i (complex), so no real closed
-- form exists in this system; it must be reported as such, not crash or
-- silently return a wrong answer.
testRecurrenceClosedFormComplexRootsUnavailable :: IO ()
testRecurrenceClosedFormComplexRootsUnavailable =
  case linearRecurrence [0, -1] [1, 0] of
    Left err -> do
      putStrLn ("FAILED: complex-root recurrence returned " ++ show err)
      exitFailure

    Right recurrence ->
      case recurrenceClosedForm recurrence of
        NoClosedForm _ -> pure ()
        ClosedForm terms -> do
          putStrLn ("FAILED: expected no closed form for a complex-root recurrence, got: " ++ show terms)
          exitFailure

-- a_n = 4 a_(n-1) - 4 a_(n-2) has a repeated characteristic root (2, with
-- multiplicity 2), which needs an n * 2^n term this module doesn't
-- produce, so it must be reported as unavailable rather than silently
-- wrong.
testRecurrenceClosedFormRepeatedRootUnavailable :: IO ()
testRecurrenceClosedFormRepeatedRootUnavailable =
  case linearRecurrence [4, -4] [1, 4] of
    Left err -> do
      putStrLn ("FAILED: repeated-root recurrence returned " ++ show err)
      exitFailure

    Right recurrence ->
      case recurrenceClosedForm recurrence of
        NoClosedForm _ -> pure ()
        ClosedForm terms -> do
          putStrLn ("FAILED: expected no closed form for a repeated-root recurrence, got: " ++ show terms)
          exitFailure

 
------------------------------------
-- AlgebraicGF: guarded self-reference and Lagrange inversion
-------------------------------------
 
-- Catalan numbers via the general guarded solver, driven by a parsed-style
-- Expr rather than hand-written Haskell self-reference. This is the same
-- equation as 'testCatalanViaSelfReference' above, so it's also a check
-- that 'solveEquation' agrees with the hand-written definition.
testSolveEquationCatalan :: IO ()
testSolveEquationCatalan =
  assertEqual
    "Catalan numbers via solveEquation (Y = 1 + x*Y^2)"
    [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
    (gfTake 10 (solveEquation catalanEquation))
 
-- The equation for ternary trees, Y = 1 + x*Y^3: a cubic equation, which
-- 'solveEquation' should handle exactly as Catalan's quadratic one, 
-- since the guarded self-reference technique doesn't care about the
-- degree of phi.
testSolveEquationTernaryTrees :: IO ()
testSolveEquationTernaryTrees =
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
testLagrangeInversionMatchesCatalan :: IO ()
testLagrangeInversionMatchesCatalan =
  case asLagrangeForm catalanEquation of
    Nothing -> do
      putStrLn "FAILED: expected Catalan's equation to be recognised as Lagrange-invertible"
      exitFailure
    Just (c, phi) ->
      assertEqual
        "Catalan numbers via Lagrange inversion"
        [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]
        (lagrangeCoefficients c phi 10)
 
-- Same cross-check for ternary trees. Lagrange inversion handles phi of
-- any degree just as well as the guarded solver does.
testLagrangeInversionMatchesTernaryTrees :: IO ()
testLagrangeInversionMatchesTernaryTrees =
  case asLagrangeForm ternaryTreesEquation of
    Nothing -> do
      putStrLn "FAILED: expected ternary trees' equation to be recognised as Lagrange-invertible"
      exitFailure
    Just (c, phi) ->
      assertEqual
        "ternary trees via Lagrange inversion"
        [1, 1, 3, 12, 55, 273, 1428, 7752, 43263, 246675]
        (lagrangeCoefficients c phi 10)
 
-- A custom equation with no additive constant (c = 0) and a mixed
-- quadratic-and-cubic phi, Y = x*(1 + Y^2 + Y^3), checked both ways:
-- against the guarded solver directly, and via Lagrange inversion.
testLagrangeInversionCustomCubic :: IO ()
testLagrangeInversionCustomCubic = do
  let equation = Mul X (Add (Add (Lit 1) (Pow Y 2)) (Pow Y 3))
      expected = [0, 1, 0, 1, 1, 2, 5, 8, 21, 42]
 
  assertEqual
    "Y = x*(1 + Y^2 + Y^3) via solveEquation"
    expected
    (gfTake 10 (solveEquation equation))
 
  case asLagrangeForm equation of
    Nothing -> do
      putStrLn "FAILED: expected Y = x*(1 + Y^2 + Y^3) to be recognised as Lagrange-invertible"
      exitFailure
    Just (c, phi) ->
      assertEqual
        "Y = x*(1 + Y^2 + Y^3) via Lagrange inversion"
        expected
        (lagrangeCoefficients c phi 10)
 
-- Y = 1 + x*Y + x*Y^2 has two separate x*(...) terms, added together
-- rather than already combined into one x*(...) node. 'asLagrangeForm'
-- must notice that x can still be factored out of both of them together
-- (x*Y + x*Y^2 = x*(Y + Y^2)), giving c = 1, phi = Y + Y^2 (not just
-- recognise equations that already have a single x*(...) term written
-- out). The resulting coefficients are checked against 'solveEquation',
-- which has no such shape restriction and computes them independently.
testLagrangeInversionMatchesMixedEquation :: IO ()
testLagrangeInversionMatchesMixedEquation = do
  let equation = Add (Add (Lit 1) (Mul X Y)) (Mul X (Pow Y 2))
      expected = [1, 2, 6, 22, 90, 394, 1806, 8558, 41586, 206098]
 
  assertEqual
    "Y = 1 + x*Y + x*Y^2 via solveEquation"
    expected
    (gfTake 10 (solveEquation equation))
 
  case asLagrangeForm equation of
    Nothing -> do
      putStrLn "FAILED: expected Y = 1 + x*Y + x*Y^2 to be recognised as Lagrange-invertible (x can be factored out of both x*(...) terms together)"
      exitFailure
    Just (c, phi) ->
      assertEqual
        "Y = 1 + x*Y + x*Y^2 via Lagrange inversion"
        expected
        (lagrangeCoefficients c phi 10)
 
-- Y = 1 + x^2*Y has x to the second power in its only non-constant
-- term, not the first, so it is genuinely not of the Y = c + x*phi(Y)
-- shape (there is no way to factor out a single x and leave phi free of
-- x). 'asLagrangeForm' must still refuse this, while 'solveEquation'
-- computes its coefficients [1, 0, 1, 0, ...] (= 1/(1-x^2)).
testAsLagrangeFormRefusesHigherXPower :: IO ()
testAsLagrangeFormRefusesHigherXPower = do
  let equation = Add (Lit 1) (Mul (Pow X 2) Y)
 
  case asLagrangeForm equation of
    Nothing -> pure ()
    Just result -> do
      putStrLn ("FAILED: expected Y = 1 + x^2*Y to be refused by asLagrangeForm, got: " ++ show result)
      exitFailure
 
  assertEqual
    "Y = 1 + x^2*Y via solveEquation"
    [1, 0, 1, 0, 1, 0, 1, 0, 1, 0]
    (gfTake 10 (solveEquation equation))


-------------------
-- Closed form for equations quadratic in the unknown
--------------------
 
-- Catalan's equation is quadratic in Y (Y = 1 + x*Y^2), so this checks
-- 'algebraicClosedForm' against both the known Catalan sequence and
-- 'solveEquation'.
testAlgebraicClosedFormCatalan :: IO ()
testAlgebraicClosedFormCatalan =
  case algebraicClosedForm catalanEquation 1 of
    Left err -> do
      putStrLn ("FAILED: Catalan via algebraicClosedForm returned " ++ err)
      exitFailure
 
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
-- denominator (2*a(x) = 2) that never vanishes. Unlike Catalan,
-- there is no removable singularity forcing one branch. The caller's
-- expected Y(0) genuinely determines which of the two roots comes back.
testAlgebraicClosedFormBranchSelection :: IO ()
testAlgebraicClosedFormBranchSelection = do
  let equation = Add (Sub X (Mul X Y)) (Pow Y 2)
 
  case algebraicClosedForm equation 1 of
    Left err -> do
      putStrLn ("FAILED: expected the Y(0)=1 branch to succeed, got " ++ err)
      exitFailure
    Right root ->
      assertEqual "Y(0) = 1 branch is the constant series 1" [1, 0, 0, 0, 0] (gfTake 5 root)
 
  case algebraicClosedForm equation 0 of
    Left err -> do
      putStrLn ("FAILED: expected the Y(0)=0 branch to succeed, got " ++ err)
      exitFailure
    Right root ->
      assertEqual "Y(0) = 0 branch is the series x" [0, 1, 0, 0, 0] (gfTake 5 root)
 
-- A Y(0) that doesn't actually satisfy the equation at x = 0 (Catalan
-- numbers do not start at 2) must be rejected outright, not silently
-- produce a series that doesn't match the recurrence.
testAlgebraicClosedFormRejectsWrongY0 :: IO ()
testAlgebraicClosedFormRejectsWrongY0 =
  case algebraicClosedForm catalanEquation 2 of
    Left _ -> pure ()
    Right result -> do
      putStrLn ("FAILED: expected Y(0)=2 to be rejected for Catalan's equation, got: " ++ show (gfTake 5 result))
      exitFailure
 
-- Ternary trees' equation (Y = 1 + x*Y^3) has no Y^2 term at all, so it
-- is not of the quadratic shape and 'asQuadraticInY' must refuse it.
testAsQuadraticInYRefusesNonQuadratic :: IO ()
testAsQuadraticInYRefusesNonQuadratic =
  case asQuadraticInY ternaryTreesEquation of
    Nothing -> pure ()
    Just result -> do
      putStrLn ("FAILED: expected ternary trees' equation to be refused by asQuadraticInY, got: " ++ show result)
      exitFailure