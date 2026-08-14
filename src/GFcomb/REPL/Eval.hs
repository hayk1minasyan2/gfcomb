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
    describeDefinition,
    showSeriesExpr
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GFComb.AlgebraicGF 
  ( Expr,
    showAlgebraicClosedForm,
    showExpr,
    solveEquation,
    isGuardedEquation,
    asLagrangeForm
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
import Data.Char (isDigit)

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
-- The four cases correspond to the ways a series can enter the
-- environment: the three @define@ forms, and the built-ins that are
-- present from the start.
data Origin
  = -- | @define fib by recurrence: ...@
    FromRecurrence LinearRecurrence
  | -- | @define C as solution of: ...@
    FromEquation Expr
  | -- | @define S = x^2/(1 - x)@
    --
    -- The first expression is what was typed and the second is what it was
    -- solved to. They differ only when the definition refers to itself:
    -- @S = 1 + A*S@ is stored as typed, and solved to @S = A/(1 - A)@.
    FromFormula SeriesExpr SeriesExpr
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
describeDefinition :: Definition -> [String]
describeDefinition definition =
  case definitionOrigin definition of
    FromBuiltin symbolicForm description ->
      [ description,
        "Generating function closed form: " ++ symbolicForm
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
       in [ "Generating function closed form: " ++ show (recurrenceRationalGF recurrence),
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
              Right rendered -> "Generating function closed form: " ++ rendered
              Left reason ->
                "Generating function closed form: not available -- "
                  ++ reason
                  ++ " (coefficients can still be computed)"
          -- An equation of the form Y = c + x*phi(Y) admits Lagrange
          -- inversion, which gives the n-th coefficient on its own without
          -- computing any of the earlier ones. Worth saying so: it is a
          -- genuinely different route to the same numbers.
          lagrangeLine =
            case asLagrangeForm equation of
              Nothing -> []
              Just (constant, phi) ->
                [ "Lagrange form: "
                    ++ name
                    ++ " = "
                    ++ showRationalPlainly constant
                    ++ " + x*phi, with phi = "
                    ++ showExpr name phi
                ]
       in [ "Defined by: " ++ name ++ " = " ++ showExpr name equation,
            generatingFunctionLine
          ]
            ++ lagrangeLine
    FromFormula typed solved ->
      -- A formula is its own closed form, so there is nothing to add when
      -- the two agree; they differ only when the definition referred to
      -- itself and was solved for.
      let solvedLine = "Generating function closed form: " ++ showSeriesExpr solved
       in if typed == solved
            then [solvedLine]
            else
              [ "Defined by: " ++ definitionName definition ++ " = " ++ showSeriesExpr typed,
                solvedLine
              ]


-- | Render a 'SeriesExpr' as source text, with parentheses only where
-- precedence requires them.
showSeriesExpr :: SeriesExpr -> String
showSeriesExpr = render (0 :: Int)
  where
    -- The precedence argument is the binding strength of the context the
    -- expression sits in, exactly as in 'GFComb.AlgebraicGF.showExpr': 1
    -- for the operands of + and -, 2 for * and /, 3 for what follows them,
    -- 4 for the base of a power.
    render precedence expression =
      case expression of
        SeriesName name -> name
        SeriesX -> "x"
        SeriesLit value ->
          parenthesiseIf
            (precedence >= 3 && (value < 0 || denominator value /= 1))
            (showRationalPlainly value)
        SeriesAdd left right ->
          parenthesiseIf (precedence > 1) (render 1 left ++ " + " ++ render 2 right)
        SeriesSub left right ->
          parenthesiseIf (precedence > 1) (render 1 left ++ " - " ++ render 2 right)
        SeriesMul left right ->
          parenthesiseIf (precedence > 2) (render 2 left ++ "*" ++ render 3 right)
        SeriesDiv left right ->
          let leftText = render 2 left
              rightText = render 3 right
              -- A number immediately followed by / and another number is
              -- lexed as a single rational literal, so "1/2" would read
              -- back as one value rather than a division. Parenthesising
              -- the left operand keeps the two apart.
              rendered
                | isLiteral left && startsWithDigit rightText =
                    "(" ++ leftText ++ ")/" ++ rightText
                | otherwise = leftText ++ "/" ++ rightText
           in parenthesiseIf (precedence > 2) rendered
        SeriesPow base power ->
          parenthesiseIf (precedence > 3) (render 4 base ++ "^" ++ show power)
 
    parenthesiseIf condition text
      | condition = "(" ++ text ++ ")"
      | otherwise = text

    isLiteral expression =
        case expression of
          SeriesLit _ -> True
          _ -> False

    startsWithDigit text =
      case text of
        first : _ -> isDigit first
        [] -> False
 
-- Does an expression refer to the given name anywhere?
mentionsName :: String -> SeriesExpr -> Bool
mentionsName name = go
  where
    go expression =
      case expression of
        SeriesName other -> other == name
        SeriesLit _ -> False
        SeriesX -> False
        SeriesAdd left right -> go left || go right
        SeriesSub left right -> go left || go right
        SeriesMul left right -> go left || go right
        SeriesDiv left right -> go left || go right
        SeriesPow base _ -> go base
 
-- | Split an expression into the pair @(A, B)@ for which it equals
-- @A + B*name@, or 'Nothing' if it is not linear in that name.
--
-- This is what lets a definition refer to itself. @S = 1 + A*S@ has no
-- need of a fixed point: rearranged, it is @S*(1 - A) = 1@, and so
-- @S = 1/(1 - A)@ -- ordinary series arithmetic, with the unknown gone.
-- That covers the sequence construction, @Seq(A) = 1/(1 - A)@, which is
-- how a great many combinatorial classes are specified.
--
-- Anything of higher degree in the name is refused, since rearranging no
-- longer removes the unknown. Those belong to @define ... as solution
-- of:@, which solves by guarded self-reference instead.
asLinearInName :: String -> SeriesExpr -> Maybe (SeriesExpr, SeriesExpr)
asLinearInName name = go
  where
    -- A subexpression that does not mention the name at all contributes
    -- only to A, whatever its shape. Checking that first keeps every case
    -- below dealing with an expression that genuinely involves the name.
    go expression
      | not (mentionsName name expression) = Just (expression, SeriesLit 0)
      | otherwise =
          case expression of
            -- Reachable only for the name itself: any other name would
            -- have been handled above.
            SeriesName _ -> Just (SeriesLit 0, SeriesLit 1)
            SeriesAdd left right -> combine SeriesAdd left right
            SeriesSub left right -> combine SeriesSub left right
            SeriesMul left right
              | not (mentionsName name left) -> do
                  (rightConstant, rightMultiplier) <- go right
                  Just (SeriesMul left rightConstant, SeriesMul left rightMultiplier)
              | not (mentionsName name right) -> do
                  (leftConstant, leftMultiplier) <- go left
                  Just (SeriesMul leftConstant right, SeriesMul leftMultiplier right)
              -- Both sides involve the name, so the product is quadratic
              -- in it at best.
              | otherwise -> Nothing
            SeriesDiv numeratorExpr denominatorExpr
              -- Dividing by something containing the name is not linear in
              -- it, and rearranging would not remove it.
              | mentionsName name denominatorExpr -> Nothing
              | otherwise -> do
                  (numeratorConstant, numeratorMultiplier) <- go numeratorExpr
                  Just
                    ( SeriesDiv numeratorConstant denominatorExpr,
                      SeriesDiv numeratorMultiplier denominatorExpr
                    )
            SeriesPow base power
              | power == 0 -> Just (SeriesLit 1, SeriesLit 0)
              | power == 1 -> go base
              | otherwise -> Nothing
            -- Literals and x cannot mention the name, so they were handled
            -- by the guard above.
            _ -> Nothing
 
    combine constructor left right = do
      (leftConstant, leftMultiplier) <- go left
      (rightConstant, rightMultiplier) <- go right
      Just (constructor leftConstant rightConstant, constructor leftMultiplier rightMultiplier)


-- Tidy an expression built by 'asLinearInName', which threads literal
-- zeros and ones through every subexpression according to whether it
-- involved the name. They are harmless to evaluate but make the rendered
-- result unreadable.
simplifySeriesExpr :: SeriesExpr -> SeriesExpr
simplifySeriesExpr expression =
  case expression of
    SeriesAdd left right -> combineAdd (simplifySeriesExpr left) (simplifySeriesExpr right)
    SeriesSub left right -> combineSub (simplifySeriesExpr left) (simplifySeriesExpr right)
    SeriesMul left right -> combineMul (simplifySeriesExpr left) (simplifySeriesExpr right)
    SeriesDiv left right -> combineDiv (simplifySeriesExpr left) (simplifySeriesExpr right)
    SeriesPow base power -> combinePow (simplifySeriesExpr base) power
    _ -> expression
  where
    combineAdd (SeriesLit 0) right = right
    combineAdd left (SeriesLit 0) = left
    combineAdd left right = SeriesAdd left right

    combineSub left (SeriesLit 0) = left
    combineSub left right = SeriesSub left right

    combineMul (SeriesLit 0) _ = SeriesLit 0
    combineMul _ (SeriesLit 0) = SeriesLit 0
    combineMul (SeriesLit 1) right = right
    combineMul left (SeriesLit 1) = left
    combineMul left right = SeriesMul left right

    combineDiv left (SeriesLit 1) = left
    combineDiv left right = SeriesDiv left right

    combinePow _ 0 = SeriesLit 1
    combinePow base 1 = base
    combinePow base power = SeriesPow base power

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
        FromFormula _ _ -> "formula"
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
    DefineByFormula name expression ->
      case asLinearInName name expression of
        Nothing -> (Output (nonLinearMessage name), env)
        Just (constantPart, multiplierPart) ->
          -- Solving A + B*name for the name gives A / (1 - B). When the
          -- definition does not mention itself B is 0, and this is just A.
          let solved
                | mentionsName name expression =
                    simplifySeriesExpr
                      (SeriesDiv constantPart (SeriesSub (SeriesLit 1) multiplierPart))
                | otherwise = expression
           in case evalSeriesExpr env solved of
                Left problem -> (Output [problem], env)
                Right series ->
                  defined
                    Definition
                      { definitionName = name,
                        definitionOrigin = FromFormula expression solved,
                        definitionSeries = series
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

    nonLinearMessage name =
      [ "'" ++ name ++ "' cannot be defined this way.",
        "A formula may refer to the name being defined, but only linearly --",
        "as in '" ++ name ++ " = 1 + A*" ++ name ++ "', which is solved as",
        "'" ++ name ++ " = A/(1 - A)' without any unknown left in it.",
        "For an equation of higher degree, use:",
        "  define " ++ name ++ " as solution of: " ++ name ++ " = 1 + x*" ++ name ++ "^2"
      ]

-- The text shown by @help@.
helpLines :: [String]
helpLines =
  [ "Commands:",
    "  define NAME by recurrence: a(n) = a(n-1) + a(n-2), a(0)=1, a(1)=1",
    "  define NAME as solution of: NAME = 1 + x*NAME^2",
    "  define NAME = x^2/(1 - x)",
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
    "The third form names a generating function outright, and may refer to",
    "names already defined:",
    "  define sums = x^2/(1 - x)",
    "  define pay = 1/((1 - x)*(1 - x^2)*(1 - x^5))",
    "",
    "Such a formula may also refer to the name being defined, provided it",
    "does so linearly -- the sequence construction, solved as 1/(1 - A):",
    "  define seq = 1 + sums*seq",
    "For anything of higher degree in the name, use 'as solution of:'.",
    "",
    "Multiplication must be written explicitly: x*C^2, not xC^2.",
    "",
    "Try 'load examples.gfcomb' for a tour."
  ]