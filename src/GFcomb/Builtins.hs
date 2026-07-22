module GFComb.Builtins
  ( BuiltinGF,

    -- Inspection
    builtinName,
    builtinDescription,
    builtinSymbolicForm,
    builtinGeneratingFunction,

    -- Available built-ins
    fibonacci,
    catalan,
    binaryTrees,
    allBuiltins,
    lookupBuiltin
  )
where

import Data.Char (toLower)
import GFComb.AlgebraicGF
  ( Expr (..),
    algebraicClosedForm,
    --solveEquation
  )
import GFComb.Core
  ( GF,
    -- gfCoeffAt,
    -- gfDivide,
    -- gfFromCoefficients,
    -- gfMul,
    -- gfOne,
    -- gfShift,
    -- gfSub
  )
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
-- Catalan numbers and binary trees
---------------------------
 
-- The equation for Catalan numbers (equally, for full binary trees),
-- where every internal node has exactly two children:
--
--   C = 1 + x*C^2
--
-- A binary tree is either a leaf, or a root with a left subtree and a
-- right subtree, each itself a binary tree (see GFComb.AlgebraicGF for
-- the general theory this kind of equation comes from).
catalanEquation :: Expr
catalanEquation = Add (Lit 1) (Mul X (Pow Y 2))
 
-- C(0) = 1: there is exactly one binary tree with no internal nodes (a
-- single leaf), which is what 'algebraicClosedForm' needs to select the
-- correct branch of the quadratic formula.
--
-- Shared between 'catalan' and 'binaryTrees' below, which both solve the
-- identical equation, so the closed form is only computed once.
catalanClosedForm :: GF
catalanClosedForm =
  case algebraicClosedForm catalanEquation 1 of
    Left err -> error ("GFComb.Builtins: invalid internal Catalan equation: " ++ err)
    Right gf -> gf
 
catalan :: BuiltinGF
catalan =
  BuiltinGF
    { builtinName = "catalan",
      builtinDescription = "Catalan numbers: 1, 1, 2, 5, 14, 42, ...",
      builtinSymbolicForm = "(1 - sqrt(1 - 4x)) / (2x)",
      builtinGeneratingFunction = catalanClosedForm
    }
 
-- Full binary trees (every internal node has exactly two children),
-- counted by number of internal nodes (the same sequence as 'catalan'),
-- since they satisfy the identical equation. Kept as a separate entry,
-- because the combinatorial meaning is different,
-- even though the numbers coincide.
binaryTrees :: BuiltinGF
binaryTrees =
  BuiltinGF
    { builtinName = "binaryTrees",
      builtinDescription = "Full binary trees by number of internal nodes: 1, 1, 2, 5, 14, 42, ...",
      builtinSymbolicForm = "(1 - sqrt(1 - 4x)) / (2x)",
      builtinGeneratingFunction = catalanClosedForm
    }

---------------------------
-- Collection and lookup
-------------------------------

-- All predefined generating functions.
allBuiltins :: [BuiltinGF]
allBuiltins = [fibonacci, catalan, binaryTrees] -- TO BE ADDED

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