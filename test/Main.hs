module Main (main) where

import Control.Monad (unless)
import GFComb.Core
import GFComb.Polynomial
import GFComb.Conversion
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


testPolynomialConversion :: IO ()
testPolynomialConversion =
  assertEqual
    "convert polynomial to generating function"
    [1, 2, 3, 0, 0, 0]
    ( gfTake 6 $
        polynomialToGF (polynomialFromList [1, 2, 3])
    )