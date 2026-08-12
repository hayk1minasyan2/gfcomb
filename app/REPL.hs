-- | The GFComb read-eval-print loop.
--
-- This module is deliberately thin. Parsing lives in
-- "GFComb.REPL.Parser" and evaluation in "GFComb.REPL.Eval", both inside
-- the library, so that both can be tested; what is left here is only the
-- part that genuinely needs 'IO': reading lines, printing them, and
-- reading a file for @load@.
module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import GFComb.REPL.Command (Command)
import GFComb.REPL.Eval
  ( Env,
    Response (..),
    evalCommand,
    initialEnv
  )
import GFComb.REPL.Parser (parseCommandLine)
import System.Console.Haskeline

main :: IO ()
main = do
  mapM_ putStrLn banner
  runInputT replSettings (loop initialEnv)

banner :: [String]
banner =
  [ "GFComb -- generating functions for combinatorics",
    "Type 'help' for the available commands, or 'quit' to leave.",
    ""
  ]

-- Command history is kept in a file, so it survives between sessions
-- rather than only within one.
replSettings :: Settings IO
replSettings = defaultSettings {historyFile = Just ".gfcomb_history"}

-- | The main loop: read a line, run it, repeat until told to stop.
--
-- 'getInputLine' returns 'Nothing' at end of input (Ctrl-D), which is
-- treated the same as @quit@.
loop :: Env -> InputT IO ()
loop env = do
  maybeLine <- getInputLine "gfcomb> "
  case maybeLine of
    Nothing -> outputStrLn "Goodbye."
    Just line -> do
      (keepGoing, nextEnv) <- runLine Nothing env line
      if keepGoing then loop nextEnv else outputStrLn "Goodbye."


-- Run a single line of input, returning whether to carry on and the
-- environment to carry on with.
--
-- The first argument describes where the line came from, and is used only
-- to label parse errors: 'Nothing' when typed at the prompt, and the file
-- and line number when running a file. Without it every error from a
-- loaded file would appear to be on line 1, because each line is parsed
-- separately and so is line 1 of its own tiny input.
--
-- A long-running command can be abandoned with Ctrl-C without losing the
-- session.
runLine :: Maybe String -> Env -> String -> InputT IO (Bool, Env)
runLine origin env line =
  handleInterrupt interrupted . withInterrupt $
    case parseCommandLine line of
      Left problem -> do
        outputStr (label ++ problem)
        pure (True, env)
      Right Nothing -> pure (True, env)
      Right (Just parsedCommand) -> runCommand env parsedCommand
  where
    label = maybe "" (++ "\n") origin
    interrupted = do
      outputStrLn "Interrupted."
      pure (True, env)

-- Act on one parsed command.
runCommand :: Env -> Command -> InputT IO (Bool, Env)
runCommand env parsedCommand =
  case evalCommand env parsedCommand of
    (Output outputLines, nextEnv) -> do
      mapM_ outputStrLn outputLines
      pure (True, nextEnv)
    (QuitRequested, nextEnv) -> pure (False, nextEnv)
    (LoadRequested path, nextEnv) -> runFile nextEnv path

-- Read a file and run each of its lines as though it had been typed.
--
-- A file may itself contain a @load@, which works; a file that loads
-- itself will recurse until the stack gives out, which is left as the
-- user's problem rather than tracked here.
runFile :: Env -> FilePath -> InputT IO (Bool, Env)
runFile env path = do
  attempt <- liftIO (try (readFile path) :: IO (Either IOException String))
  case attempt of
    Left ioProblem -> do
      outputStrLn ("cannot read " ++ path ++ ": " ++ show ioProblem)
      pure (True, env)
    Right contents -> runFileLines env path (zip [1 :: Int ..] (lines contents))

runFileLines :: Env -> FilePath -> [(Int, String)] -> InputT IO (Bool, Env)
runFileLines env _ [] = pure (True, env)
runFileLines env path ((lineNumber, line) : remaining) = 
  do
      (keepGoing, nextEnv) <- runLine (Just origin) env line
      if keepGoing
        then runFileLines nextEnv path remaining
        else pure (False, nextEnv)
  where
    origin = "in " ++ path ++ ", line " ++ show lineNumber ++ ":"