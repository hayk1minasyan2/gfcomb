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
    evalSeriesExpr,

     -- * Running commands
    Response (..),
    evalCommand,
    describeDefinition
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GFComb.AlgebraicGF 
  ( Expr,
    showAlgebraicClosedForm,
    showExpr,
    solveEquation,
    isGuardedEquation
  )
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
    gfVariable,
    gfCoeffAtMaybe,
    gfConstantTerm,
    gfTake
  )
import GFComb.REPL.Command (SeriesExpr (..), Command (..))
import GFComb.Recurrence 
  ( LinearRecurrence,
    ClosedFormResult (..),
    recurrenceClosedForm,
    recurrenceGF,
    recurrenceRationalGF,
    showClosedForm
  )
import Data.List (intercalate)
import Data.Ratio (denominator, numerator)

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


---------------------------------------
-- Responses
----------------------------------------

-- | What the REPL should do with the result of a command.
--
-- Most commands just produce lines to print. The two exceptions are the
-- ones that cannot be carried out purely: @load@ needs to read a file, and
-- @quit@ needs to stop the loop. Rather than performing either here, this
-- type reports the intention and lets the loop act on it - which is what
-- keeps 'evalCommand' free of 'IO' and directly testable.
data Response
  = -- | Lines to print, in order.
    Output [String]
  | -- | The loop should read this file and run each of its lines.
    LoadRequested FilePath
  | -- | The loop should stop.
    QuitRequested
  deriving (Eq, Show)

----------------------------------------
-- Describing a definition
----------------------------------------

-- | The lines describing one definition, as shown by @show@ and echoed
-- when it is first defined.
--
-- Built-ins carry their symbolic form directly. A recurrence can always
-- produce a rational generating function, and often a closed form too. An
-- equation can always produce coefficients, but only yields a symbolic
-- generating function when it is quadratic in the unknown.
describeDefinition :: Definition -> [String]
describeDefinition definition =
  case definitionOrigin definition of
    FromBuiltin symbolicForm description ->
      [ description,
        "Generating function: " ++ symbolicForm
      ]
    FromRecurrence recurrence ->
      -- 'showClosedForm' already renders both cases, but its "No closed form
      -- available: ..." wording reads oddly under a "Closed form:" label, so
      -- the two are distinguished here. Matching only 'NoClosedForm' and
      -- leaving the rest to a catch-all keeps this independent of how many
      -- fields the success case carries.
      let closedFormLine =
            case recurrenceClosedForm recurrence of
              NoClosedForm reason -> "Closed form: not available -- " ++ reason
              result -> "Closed form: " ++ showClosedForm result
       in [ "Generating function: " ++ show (recurrenceRationalGF recurrence),
            closedFormLine
          ]
    FromEquation equation ->
      -- The expected value of Y(0) is not asked of the user: it is simply
      -- the constant term of the series 'solveEquation' has already
      -- produced.
      let name = definitionName definition
          constantTerm = gfConstantTerm (definitionSeries definition)
          generatingFunctionLine =
            case showAlgebraicClosedForm equation constantTerm of
              Right rendered -> "Generating function: " ++ rendered
              Left reason ->
                "Generating function: no closed form available -- "
                  ++ reason
                  ++ " (coefficients can still be computed)"
       in [ "Defined by: " ++ name ++ " = " ++ showExpr name equation,
            generatingFunctionLine
          ]

-- The first few coefficients of a definition, as a line of output.
--
-- Shown when a definition is made as well as by @show@, because for an
-- equation with no closed form it is the only informative output there is.
firstCoefficientsLine :: Definition -> String
firstCoefficientsLine definition =
  "First 10 coefficients: " ++ showRationalList (gfTake 10 (definitionSeries definition))

-- A list of rationals written the way they would be typed, so that whole
-- numbers appear as @1@ rather than @1 % 1@.
showRationalList :: [Rational] -> String
showRationalList values = "[" ++ intercalate ", " (map showRationalPlainly values) ++ "]"

showRationalPlainly :: Rational -> String
showRationalPlainly value
  | denominator value == 1 = show (numerator value)
  | otherwise = show (numerator value) ++ "/" ++ show (denominator value)

-- Every defined name, tagged with how it was defined.
listing :: Env -> [String]
listing env =
  case envNames env of
    [] -> ["Nothing is defined."]
    names -> "Defined names:" : map describeName names
  where
    describeName name =
      "  " ++ name ++ case envLookup name env of
        Just definition -> " (" ++ originLabel (definitionOrigin definition) ++ ")"
        Nothing -> ""

    originLabel origin =
      case origin of
        FromRecurrence _ -> "recurrence"
        FromEquation _ -> "equation"
        FromBuiltin _ _ -> "built-in"

----------------------------------------
-- Running a command
----------------------------------------

-- | Run one command against the environment, producing what to show and
-- the environment to carry forward.
--
-- Pure: nothing is printed and no file is read here. A command that needs
-- either reports it through 'Response' instead.
evalCommand :: Env -> Command -> (Response, Env)
evalCommand env cmd =
  case cmd of
    Help -> (Output helpLines, env)
    Quit -> (QuitRequested, env)
    Load path -> (LoadRequested path, env)
    ListNames -> (Output (listing env), env)
    ShowName name ->
      case envLookup name env of
        Nothing -> (Output [notDefined name], env)
        Just definition -> (Output (fullDescription definition), env)
    DefineByRecurrence name recurrence ->
      defined
        Definition
          { definitionName = name,
            definitionOrigin = FromRecurrence recurrence,
            definitionSeries = recurrenceGF recurrence
          }
    DefineByEquation name equation
      -- Checked before solving, not after: 'solveEquation' does not fail on
      -- an unguarded equation, it fails to terminate.
      | not (isGuardedEquation equation) -> (Output (unguardedMessage name), env)
      | otherwise ->
          defined
            Definition
              { definitionName = name,
                definitionOrigin = FromEquation equation,
                definitionSeries = solveEquation equation
              }
    Coeffs expression count ->
      (Output (withSeries expression (\series -> [showRationalList (gfTake count series)])), env)
    CoeffAt expression index ->
      (Output (withSeries expression (coefficientAt index)), env)
  where
    fullDescription definition =
      describeDefinition definition ++ [firstCoefficientsLine definition]

    defined definition =
      (Output (fullDescription definition), envInsert definition env)

    -- Evaluate a query expression and hand the resulting series to a
    -- renderer, or report why it could not be evaluated. Both 'Coeffs' and
    -- 'CoeffAt' need exactly this, and differ only in the renderer.
    withSeries expression render =
      case evalSeriesExpr env expression of
        Left problem -> [problem]
        Right series -> render series

    coefficientAt index series =
      case gfCoeffAtMaybe series index of
        Nothing -> ["a coefficient index cannot be negative"]
        Just value -> [showRationalPlainly value]

    notDefined name = "'" ++ name ++ "' is not defined -- use 'list' to see what is"

    unguardedMessage name =
      [ "'" ++ name ++ "' cannot be solved as written.",
        "Every occurrence of '" ++ name ++ "' on the right must be multiplied by x,",
        "so that each coefficient depends only on earlier ones.",
        "For example '" ++ name ++ " = 1 + x*" ++ name ++ "^2' works,",
        "but '" ++ name ++ " = 1 + " ++ name ++ "^2' does not."
      ]

-- The text shown by @help@.
helpLines :: [String]
helpLines =
  [ "Commands:",
    "  define NAME by recurrence: a(n) = a(n-1) + a(n-2), a(0)=1, a(1)=1",
    "  define NAME as solution of: NAME = 1 + x*NAME^2",
    "  coeffs EXPR N   the first N coefficients of EXPR",
    "  coeff EXPR N    the coefficient of x^N in EXPR",
    "  add A B         the first 10 coefficients of A + B",
    "  list            every defined name",
    "  show NAME       describe one definition",
    "  load FILE       run the commands in a file",
    "  help            this message",
    "  quit, exit      leave the REPL",
    "",
    "An EXPR combines defined names with + - * / and ^, and may mention x:",
    "  coeffs catalan + fibonacci 10",
    "  coeffs 1/(1 - x) 5",
    "",
    "In a recurrence, the name on the left is yours to choose, and gaps are",
    "allowed: a(n) = a(n-1) + a(n-3) has order 3 with a zero coefficient for",
    "a(n-2). Every initial value a(0) .. a(k-1) must be given, where k is the",
    "largest offset referred to.",
    "",
    "The right-hand side may also contain a polynomial in n:",
    "  define hanoi by recurrence: a(n) = 2*a(n-1) + 1, a(0)=1",
    "  define costs by recurrence: a(n) = 2*a(n-1) + n, a(0)=0",
    "",
    "With no reference to an earlier term it is simply a formula, and takes",
    "no initial values, since every value is already determined:",
    "  define squares by recurrence: a(n) = n^2",
    "",
    "Multiplication must be written explicitly: x*C^2, not xC^2.",
    "",
    "Try 'load examples.gfcomb' for a tour."
  ]