module Main (main) where

import GFComb.Builtins
    ( BuiltinGF,
        allBuiltins,
        builtinDescription,
        builtinGeneratingFunction,
        builtinName,
        builtinSymbolicForm,
        lookupBuiltin
    )

import GFComb.Core (gfTake)

import Data.Ratio
    ( denominator,
        numerator
    )

import System.IO
    ( hFlush,
        stdout
    )

main :: IO ()
main = do
    putStrLn "GFComb - Combinatorial Generating Functions"
    putStrLn "Type 'help' to see available commands."
    replLoop

replLoop :: IO ()
replLoop = do
    putStr "gfcomb> "
    hFlush stdout

    input <- getLine

    shouldContinue <- executeCommand (words input)

    if shouldContinue
        then replLoop
    else putStrLn "Goodbye."

executeCommand :: [String] -> IO Bool
executeCommand [] = pure True

executeCommand ["help"] = do
    printHelp
    pure True

executeCommand ["list"] = do
    printBuiltins
    pure True

executeCommand ["quit"] =
    pure False

executeCommand ["exit"] =
    pure False

executeCommand ["show", name] = do
    showBuiltin name
    pure True

executeCommand _ = do
    putStrLn "Unknown command. Type 'help' to see available commands."
    pure True

printHelp :: IO ()
printHelp = do
    putStrLn "Available commands:"
    putStrLn "  help    Display this help message"
    putStrLn "  list    List predefined generating functions"
    putStrLn "  show NAME    Show information about a predefined GF"
    putStrLn "  quit    Exit GFComb"

printBuiltins :: IO ()
printBuiltins = do
    putStrLn "Available predefined generating functions:"
    mapM_
        (\builtin -> putStrLn ("  " ++ builtinName builtin))
        allBuiltins

showBuiltin :: String -> IO ()
showBuiltin requestedName =
    case lookupBuiltin requestedName of
        Nothing ->
            putStrLn
                ( "Unknown predefined generating function: "
                    ++ requestedName)
        Just builtin -> printBuiltinDetails builtin


printBuiltinDetails :: BuiltinGF -> IO ()
printBuiltinDetails builtin = do
    putStrLn ("Name: " ++ builtinName builtin)
    putStrLn ("Description: " ++ builtinDescription builtin)
    putStrLn ("Generating function: " ++ builtinSymbolicForm builtin)
    putStrLn
        ( "First 10 coefficients: "
            ++ showRationalList
                ( gfTake
                    10
                    (builtinGeneratingFunction builtin)
                )
        )



-------------------
-- Helper for show
--------------------

showRational :: Rational -> String
showRational value
    | denominator value == 1 = show (numerator value)
    | otherwise = show (numerator value)
            ++ "/"
            ++ show (denominator value)

showRationalList :: [Rational] -> String
showRationalList values = "[" ++ joinWithComma (map showRational values) ++ "]"

joinWithComma :: [String] -> String
joinWithComma [] = ""
joinWithComma [value] = value
joinWithComma (value : remainingValues) = value ++ ", " ++ joinWithComma remainingValues