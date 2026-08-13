-- | The abstract syntax of REPL input: the commands a user can type, and
-- the small expression language for combining already-defined generating
-- functions.
--
-- This module is deliberately free of any evaluation logic, so that the
-- parser (which produces these values) and the evaluator (which consumes
-- them) share a description of the language without depending on each
-- other.
module GFComb.REPL.Command
  ( -- * Commands
    Command (..),

    -- * Series expressions
    SeriesExpr (..)
  )
where

import GFComb.AlgebraicGF (Expr)
import GFComb.Recurrence (LinearRecurrence)
import Numeric.Natural (Natural)

-- | A single line of REPL input, once parsed.
--
-- There are two ways to define a generating function, mirroring the two
-- solvers in the library: 'DefineByRecurrence' feeds "GFComb.Recurrence",
-- and 'DefineByEquation' feeds "GFComb.AlgebraicGF".
--
-- Note that there is no constructor for the @add@ command: @add A B@ is
-- sugar, and the parser turns it directly into
-- @'Coeffs' ('SeriesAdd' a b) 10@. Keeping it out of this type means the
-- evaluator has one code path for coefficient queries rather than two that
-- must be kept in agreement.
data Command
  = -- | @help@ -- list the available commands.
    Help
  | -- | @list@ -- list every name currently defined, including built-ins.
    ListNames
  | -- | @show NAME@ -- describe one definition.
    ShowName String
  | -- | @define C as solution of: C = 1 + x*C^2@
    --
    -- The 'String' is the name being defined. Within the 'Expr' that name
    -- has already been resolved to 'GFComb.AlgebraicGF.Y', so the equation
    -- is self-contained and can go straight to
    -- 'GFComb.AlgebraicGF.solveEquation'.
    DefineByEquation String Expr
  | -- | @define fib by recurrence: a(n) = a(n-1) + a(n-2), a(0)=1, a(1)=1@
    --
    -- The 'String' is the name being defined; the placeholder used inside
    -- the recurrence itself (@a@ above) is local to that syntax and is not
    -- retained.
    --
    -- Building the 'LinearRecurrence' is the parser's job, which means the
    -- parser is what checks that the number of initial values matches the
    -- recurrence's order. That check has to happen before the value
    -- exists, because 'GFComb.Recurrence.linearRecurrence' pairs each
    -- coefficient with its initial value and so cannot represent a
    -- mismatch at all.
    DefineByRecurrence String LinearRecurrence
  | -- | @define S = x^2/(1 - x)@ — define a name by an explicit formula.
    --
    -- The expression may refer to the name being defined, provided it does
    -- so linearly: @S = 1 + A*S@ is solved as @S = A\/(1 - A)@. Anything of
    -- higher degree belongs to 'DefineByEquation'.
    DefineByFormula String SeriesExpr
  | -- | @coeffs fib 10@ -- the first N coefficients of an expression.
    Coeffs SeriesExpr Int
  | -- | @coeff fib 20@ -- the exact coefficient of @x^N@.
    --
    -- Note the different meaning of the number here and in 'Coeffs'.
    -- Here it is an index.
    CoeffAt SeriesExpr Int
  | -- | @load FILE@ -- run each line of a file as though it had been typed.
    Load FilePath
  | -- | @quit@ or @exit@.
    Quit
  deriving (Eq, Show)

-- | An expression combining generating functions that already exist.
--
-- This is a different language from 'Expr'(deliberately). An 'Expr'
-- describes a functional equation that is being solved, so it may refer to
-- its own unknown. A 'SeriesExpr' only ever refers to series that are
-- already known, so it cannot be self-referential.
data SeriesExpr
  = -- | A defined name, e.g. @catalan@.
    SeriesName String
  | -- | A rational constant, e.g. @3@ or @1\/2@.
    SeriesLit Rational
  | -- | The variable @x@ itself.
    SeriesX
  | SeriesAdd SeriesExpr SeriesExpr
  | SeriesSub SeriesExpr SeriesExpr
  | SeriesMul SeriesExpr SeriesExpr
  | -- | Division, which fails at evaluation time if the divisor's constant
    -- term is 0 (see 'GFComb.Core.gfDivide').
    SeriesDiv SeriesExpr SeriesExpr
  | -- | A natural-number power. As in 'Expr', the exponent is a 'Natural'
    -- so that a negative exponent cannot be represented at all.
    SeriesPow SeriesExpr Natural
  deriving (Eq, Show)