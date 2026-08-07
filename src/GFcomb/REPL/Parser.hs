-- | Parsing REPL input with @megaparsec@.
--
-- Three languages are parsed here:
--
--   * 'equationExpr' -- the right-hand side of @define ... as solution of:@,
--     producing an 'Expr' in which the name being defined has become
--     'GFComb.AlgebraicGF.Y'.
--   * 'recurrenceBody' -- the body of @define ... by recurrence:@,
--     producing a 'LinearRecurrence'.
--   * 'seriesExpr' -- a query expression over already-defined names,
--     producing a 'SeriesExpr'.
--
-- Multiplication must be written explicitly: @x*T^2@, not @xT^2@. 
module GFComb.REPL.Parser
  ( 
    -- * Parser type
    Parser
  )
where

import Control.Monad (unless)
import Data.Ratio ((%))
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- | The parser type synonym used throughout
type Parser = Parsec Void String

----------------------------------------
-- Parser primitives
----------------------------------------

-- Skip whitespace and @--@ line comments.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") empty

-- Run a parser and then consume any trailing whitespace, so that every
-- parser in this module can assume it starts at a non-space character.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- Match a fixed string, consuming trailing whitespace.
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- Match a fixed word that must not run straight into a longer identifier.
--
-- This is what keeps @coeff@ from matching the start of @coeffs@, and
-- @n@ from matching the start of @next@.
keyword :: String -> Parser ()
keyword word =
  lexeme (try (string word *> notFollowedBy (alphaNumChar <|> char '_'))) <?> word

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Names that may not be used as identifiers: every command keyword, the
-- words used by the two @define@ forms, and @x@ itself. 
reservedWords :: [String]
reservedWords =
  [ "help", "list", "show", "define", "coeffs", "coeff", "add", "load",
    "quit", "exit", "by", "as", "solution", "of", "recurrence", "x"
  ]

-- An identifier: a letter followed by letters, digits, or underscores, and
-- not a reserved word.
identifier :: Parser String
identifier = lexeme (try identifierCharacters) <?> "name"
  where
    identifierCharacters = do
      firstCharacter <- letterChar
      remainingCharacters <- many (alphaNumChar <|> char '_')
      let name = firstCharacter : remainingCharacters
      if name `elem` reservedWords
        then fail ("'" ++ name ++ "' is reserved and cannot be used as a name")
        else pure name

-- An identifier that must be one particular name.
specificName :: String -> Parser ()
specificName expected = do
  name <- identifier
  unless (name == expected) $
    fail ("expected '" ++ expected ++ "' here, but found '" ++ name ++ "'")

-- The series variable @x@.
--
-- The 'notFollowedBy' matters: without it this would match the leading
-- @x@ of a name like @xs@, silently splitting one identifier in two.
variableX :: Parser ()
variableX = lexeme (try (char 'x' *> notFollowedBy (alphaNumChar <|> char '_'))) <?> "x"

-- A non-negative integer.
naturalLiteral :: Parser Natural
naturalLiteral = lexeme L.decimal <?> "a whole number"

-- A rational literal: a whole number, optionally followed by @/@ and a
-- second whole number.
--
-- The 'try' is what lets @1\/2@ and @catalan\/2@ exist at the same time: if what follows
-- the slash is not a number, the slash is left for the division operator
-- to pick up instead.
rationalLiteral :: Parser Rational
rationalLiteral = do
  numeratorValue <- lexeme L.decimal
  maybeDenominator <- optional (try (symbol "/" *> lexeme L.decimal))
  case maybeDenominator of
    Nothing -> pure (fromInteger numeratorValue)
    Just 0 -> fail "a rational literal cannot have a zero denominator"
    Just denominatorValue -> pure (numeratorValue % denominatorValue)

-- A rational literal that may carry a leading sign.
signedRationalLiteral :: Parser Rational
signedRationalLiteral = do
  sign <- option 1 ((-1) <$ symbol "-" <|> 1 <$ symbol "+")
  value <- rationalLiteral
  pure (sign * value)
