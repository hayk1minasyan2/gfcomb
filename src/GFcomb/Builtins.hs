module GFComb.Builtins
  ( BuiltinGF,

    -- Inspection
    builtinName,
    builtinDescription,
    builtinSymbolicForm,
    builtinGeneratingFunction,

    -- Available built-ins
    fibonacci,
    allBuiltins,
    lookupBuiltin
  )
where

import Data.Char (toLower)
import GFComb.Core (GF)
import GFComb.Recurrence
  ( linearRecurrence,
    recurrenceGF,
    recurrenceRationalGF
  )

--------------------------------------
-- Built-in generating functions
----------------------------------------

-- A named generating function included in the GFComb library.
--
-- The constructor is hidden so that built-in entries can only be created inside this module.

data BuiltinGF =
  BuiltinGF
    { builtinName :: String,
      builtinDescription :: String,
      builtinSymbolicForm :: String,
      builtinGeneratingFunction :: GF
    }

instance Show BuiltinGF where
  show builtin =
    builtinName builtin ++ ": " ++ builtinSymbolicForm builtin

----------------------------------------
-- Fibonacci numbers
--------------------------------------------

-- The Fibonacci sequence
--
-- 1, 1, 2, 3, 5, 8, ...
--
-- satisfying
--
-- a_n = a_(n-1) + a_(n-2)
--
-- with initial values a_0 = 1 and a_1 = 1.

fibonacci :: BuiltinGF
fibonacci =
  case linearRecurrence [1, 1] [1, 1] of
    Left err ->
      error
        ( "GFComb.Builtins: invalid internal Fibonacci recurrence: "
            ++ show err
        )

    Right recurrence ->
      BuiltinGF
        { builtinName = "fibonacci",
          builtinDescription = "Fibonacci numbers: 1, 1, 2, 3, 5, 8, ...",
          builtinSymbolicForm =
            show (recurrenceRationalGF recurrence),
          builtinGeneratingFunction =
            recurrenceGF recurrence
        }

---------------------------
-- Collection and lookup
-------------------------------

-- All predefined generating functions.
allBuiltins :: [BuiltinGF]
allBuiltins = [fibonacci] -- TO BE ADDED

-- Find a built-in generating function by name.
--
-- Lookup is case-insensitive.
lookupBuiltin :: String -> Maybe BuiltinGF
lookupBuiltin requestedName = findByName allBuiltins
  where
    normalizedRequestedName = map toLower requestedName

    findByName :: [BuiltinGF] -> Maybe BuiltinGF
    findByName [] = Nothing
    findByName (builtin : remainingBuiltins)
      | map toLower (builtinName builtin) == normalizedRequestedName = Just builtin
      | otherwise = findByName remainingBuiltins