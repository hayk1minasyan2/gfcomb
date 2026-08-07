-- | The REPL's environment, and evaluation of query expressions against it.
--
-- Everything here is pure. No 'IO', no printing. This is what allows the
-- evaluator to be tested directly, and it keeps the decision about how to
-- display a result separate from the decision about what the result is.
module GFComb.REPL.Eval
  ( -- * Definitions
    Definition (..),
    Origin (..),

    -- * The environment
    Env,
    emptyEnv,
    initialEnv,
    envLookup,
    envInsert,
    envNames,

    -- * Evaluating query expressions
    evalSeriesExpr
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GFComb.AlgebraicGF (Expr)
import GFComb.Builtins
  ( allBuiltins,
    builtinDescription,
    builtinGeneratingFunction,
    builtinName,
    builtinSymbolicForm
  )
import GFComb.Core
  ( GF,
    GFError (..),
    gfAdd,
    gfConstant,
    gfDivide,
    gfMul,
    gfPow,
    gfSub,
    gfVariable
  )
import GFComb.REPL.Command (SeriesExpr (..))
import GFComb.Recurrence (LinearRecurrence)

------------------------------
-- Definitions
-----------------------------

-- | One named generating function known to the REPL.
--
-- The series is kept alongside the 'Origin' rather than derived from it on
-- demand, so that a definition is solved once and then reused. Laziness
-- means this costs nothing until coefficients are actually asked for.
data Definition = Definition
  { definitionName :: String,
    -- | Where the definition came from, kept in structured form so that
    -- @show@ can describe it without having to re-parse anything.
    definitionOrigin :: Origin,
    definitionSeries :: GF
  }

-- | How a definition came to exist.
--
-- The three cases correspond to the three ways a series can enter the
-- environment: the two @define@ forms, and the built-ins that are present
-- from the start.
data Origin
  = -- | @define fib by recurrence: ...@
    FromRecurrence LinearRecurrence
  | -- | @define C as solution of: ...@
    FromEquation Expr
  | -- | One of "GFComb.Builtins"' entries, carrying its symbolic form and
    -- its description.
    FromBuiltin String String

------------------------------------
-- The environment
------------------------------------

-- | The set of names currently defined.
newtype Env = Env (Map String Definition)

-- | An environment with nothing defined at all.
--
-- Mostly useful for testing; a session starts from 'initialEnv'.
emptyEnv :: Env
emptyEnv = Env Map.empty

-- | The environment a session starts with: every built-in, already defined.
--
-- Putting the built-ins straight into the environment means there is one
-- lookup path rather than two, so @coeffs catalan 10@ needs no special
-- handling to find a built-in rather than a user definition.
initialEnv :: Env
initialEnv = Env (Map.fromList [(builtinName b, definitionFor b) | b <- allBuiltins])
  where
    definitionFor b =
      Definition
        { definitionName = builtinName b,
          definitionOrigin = FromBuiltin (builtinSymbolicForm b) (builtinDescription b),
          definitionSeries = builtinGeneratingFunction b
        }

-- | Look up one name.
envLookup :: String -> Env -> Maybe Definition
envLookup name (Env definitions) = Map.lookup name definitions

-- | Add a definition, replacing any existing one of the same name.
envInsert :: Definition -> Env -> Env
envInsert definition (Env definitions) =
  Env (Map.insert (definitionName definition) definition definitions)

-- | Every defined name, in alphabetical order.
envNames :: Env -> [String]
envNames (Env definitions) = Map.keys definitions

----------------------------------------
-- Evaluating query expressions
----------------------------------------

-- | Evaluate a query expression to a formal power series.
--
-- Fails with a readable message if a name is not defined, or if a division
-- is not valid as a formal power series.
--
-- >>> fmap (gfTake 5) (evalSeriesExpr initialEnv (SeriesName "catalan"))
-- Right [1 % 1,1 % 1,2 % 1,5 % 1,14 % 1]
--
-- >>> evalSeriesExpr initialEnv (SeriesName "undefName")
-- Left "'undefName' is not defined -- use 'list' to see what is"
evalSeriesExpr :: Env -> SeriesExpr -> Either String GF
evalSeriesExpr env = evaluate
  where
    evaluate expression =
      case expression of
        SeriesName name ->
          case envLookup name env of
            Nothing -> Left ("'" ++ name ++ "' is not defined -- use 'list' to see what is")
            Just definition -> Right (definitionSeries definition)
        SeriesLit value -> Right (gfConstant value)
        SeriesX -> Right gfVariable
        SeriesAdd left right -> gfAdd <$> evaluate left <*> evaluate right
        SeriesSub left right -> gfSub <$> evaluate left <*> evaluate right
        SeriesMul left right -> gfMul <$> evaluate left <*> evaluate right
        SeriesPow base exponent' -> (`gfPow` exponent') <$> evaluate base
        SeriesDiv numeratorExpr denominatorExpr -> do
          numeratorSeries <- evaluate numeratorExpr
          denominatorSeries <- evaluate denominatorExpr
          case gfDivide numeratorSeries denominatorSeries of
            Right result -> Right result
            Left DivisionByZeroConstant ->
              Left
                ( "cannot divide by a series whose constant term is 0 "
                    ++ "(division of formal power series is only defined otherwise)"
                )
            Left otherError -> Left ("cannot divide: " ++ show otherError)