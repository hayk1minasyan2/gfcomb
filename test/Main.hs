module Main (main) where

import Control.Monad (unless)
import GFComb.Core
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "Running GFComb core tests..."

  putStrLn "Construction..."
  testConstruction

  putStrLn "Addition..."
  testAddition

  putStrLn "Multiplication..."
  testMultiplication

  putStrLn "Derivative..."
  testDerivative

  putStrLn "Integral..."
  testIntegral

  putStrLn "Division..."
  testDivision

  putStrLn "Composition..."
  testComposition

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