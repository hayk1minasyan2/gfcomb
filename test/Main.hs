module Main (main) where

import Control.Monad (unless)
import GFComb.Core
import GFComb.Polynomial
import GFComb.Conversion
import GFComb.RationalGF
import GFComb.Recurrence
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