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
import Numeric.Natural (Natural)

-- | A single line of REPL input, once parsed.
--
-- Note that there is no constructor for the @add@ command: @add A B@ is
-- sugar, and the parser turns it directly into
-- @'Coeffs' 10 ('SeriesAdd' a b)@. Keeping it out of this type means the
-- evaluator has one code path for coefficient queries rather than two
-- that must be kept in agreement.
data Command
  = -- | @help@ — list the available commands.
    Help
  | -- | @list@ — list every name currently defined, including built-ins.
    ListNames
  | -- | @show NAME@ — describe one definition: its equation (or symbolic
    -- form) and its first few coefficients.
    ShowName String
  | -- | @define T = 1 + x*T^2@ — introduce a new generating function by a
    -- functional equation. The 'String' is the name being defined, and
    -- within the 'Expr' that same name has been resolved to
    -- 'GFComb.AlgebraicGF.Y' (the unknown), so the equation is
    -- self-contained.
    Define String Expr
  | -- | @coeffs N EXPR@ — the first @N@ coefficients of @EXPR@.
    Coeffs Int SeriesExpr
  | -- | @coeff N EXPR@ — the single coefficient of @x^N@ in @EXPR@.
    --
    -- Note the different meaning of @N@ here and in 'Coeffs'
    -- Here @N@ is an index.
    CoeffAt Int SeriesExpr
  | -- | @load FILE@ — run each line of a file as though it had been typed.
    Load FilePath
  | -- | @quit@ or @exit@.
    Quit
  deriving (Eq, Show)

-- | An expression combining generating functions that already exist.
--
-- This is a different language from 'Expr' (deliberately). An 'Expr'
-- describes a functional equation that is being solved, so it may refer to
-- its own unknown; a 'SeriesExpr' only ever refers to series that are
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