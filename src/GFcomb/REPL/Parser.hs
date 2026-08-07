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
  ( -- * Entry points
    parseCommand,
    parseEquationRhs,
    parseSeriesExpr,
    parseRecurrenceBody,
 
    -- * Individual parsers
    command,
    equationExpr,
    seriesExpr,
    recurrenceBody,
 
    -- * Parser type
    Parser
  )
where

import Control.Monad (unless, when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isSpace)
import Data.List (intercalate, nub)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Void (Void)
import GFComb.AlgebraicGF (Expr (..))
import GFComb.REPL.Command (Command (..), SeriesExpr (..))
import GFComb.Recurrence (LinearRecurrence, linearRecurrence)
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



-----------------------------------
-- Equation right-hand sides
-----------------------------------
 
-- | Parse the right-hand side of an @as solution of:@ definition, given the
-- name being defined.
--
-- That name is resolved to 'Y' as it is parsed, so the resulting 'Expr' is
-- self-contained and can be handed straight to
-- 'GFComb.AlgebraicGF.solveEquation'. Any other name is rejected: a
-- definition may refer only to @x@ and to itself.
equationExpr :: String -> Parser Expr
equationExpr unknownName =
  makeExprParser (equationTerm unknownName) equationOperators
 
-- An atom, followed by any number of @^ n@ exponents.
--
-- Exponents are natural-number literals rather than arbitrary expressions,
-- because 'Pow' takes a 'Natural', so a symbolic or negative exponent
-- cannot even be represented.
equationTerm :: String -> Parser Expr
equationTerm unknownName = do
  base <- equationAtom unknownName
  exponents <- many (symbol "^" *> naturalLiteral)
  pure (foldl Pow base exponents)
 
equationAtom :: String -> Parser Expr
equationAtom unknownName =
  parens (equationExpr unknownName)
    <|> (X <$ variableX)
    <|> (Lit <$> rationalLiteral)
    <|> theUnknown
  where
    theUnknown = do
      name <- identifier
      if name == unknownName
        then pure Y
        else
          fail
            ( "unknown name '"
                ++ name
                ++ "' -- the right-hand side of a definition may only mention 'x' and '"
                ++ unknownName
                ++ "' itself"
            )
 
equationOperators :: [[Operator Parser Expr]]
equationOperators =
  [ [Prefix (negated <$ symbol "-")],
    [InfixL (Mul <$ symbol "*")],
    [InfixL (Add <$ symbol "+"), InfixL (Sub <$ symbol "-")]
  ]
  where
    negated = Sub (Lit 0)
 


-------------------------
-- Recurrences
-------------------------
 
-- | Parse the body of a @by recurrence:@ definition, e.g.
--
-- > a(n) = a(n-1) + a(n-2), a(0)=1, a(1)=1
--
-- The name used for the sequence (@a@ above) is whatever appears on the
-- left, and every later mention must match it - so @fib(n) = fib(n-1) + ...@
-- works just as well.
--
-- Gaps are allowed: @a(n) = a(n-1) + a(n-3)@ has order 3 with a zero
-- coefficient for @a(n-2)@. Repeated offsets are summed, so
-- @a(n-1) + a(n-1)@ means a coefficient of 2.
--
-- The number of initial values must match the order exactly. That check
-- happens here rather than later because
-- 'GFComb.Recurrence.linearRecurrence' pairs each coefficient with its
-- initial value, and so cannot represent a mismatch at all.
recurrenceBody :: Parser LinearRecurrence
recurrenceBody = do
  placeholder <- identifier
  _ <- parens (keyword "n")
  _ <- symbol "="
  terms <- recurrenceTerms placeholder
  _ <- symbol ","
  initialValues <- initialValue placeholder `sepBy1` symbol ","
  buildRecurrence terms initialValues
 
-- A sum of terms like @3*a(n-1)@ or @a(n-2)@, with signs.
recurrenceTerms :: String -> Parser [(Int, Rational)]
recurrenceTerms placeholder = do
  leadingSign <- option 1 ((-1) <$ symbol "-" <|> 1 <$ symbol "+")
  firstTerm <- recurrenceTerm placeholder
  remainingTerms <- many signedTerm
  pure (applySign leadingSign firstTerm : remainingTerms)
  where
    signedTerm = do
      sign <- ((-1) <$ symbol "-") <|> (1 <$ symbol "+")
      term <- recurrenceTerm placeholder
      pure (applySign sign term)
 
    applySign sign (offset, coefficient) = (offset, sign * coefficient)
 
-- One term: an optional rational coefficient, then @a(n-k)@.
recurrenceTerm :: String -> Parser (Int, Rational)
recurrenceTerm placeholder = do
  coefficient <- option 1 (try (rationalLiteral <* symbol "*"))
  offset <- termOffset placeholder
  pure (offset, coefficient)
 
-- The @a(n-k)@ part of a term, returning k.
termOffset :: String -> Parser Int
termOffset placeholder = do
  specificName placeholder
  parens $ do
    keyword "n"
    _ <- symbol "-"
    offset <- naturalLiteral
    when (offset == 0) $
      fail
        ( "'"
            ++ placeholder
            ++ "(n-0)' refers to the term being defined, which would make the recurrence circular"
        )
    pure (fromIntegral offset)
 
-- One initial value, e.g. @a(0)=1@.
initialValue :: String -> Parser (Int, Rational)
initialValue placeholder = do
  specificName placeholder
  index <- parens naturalLiteral
  _ <- symbol "="
  value <- signedRationalLiteral
  pure (fromIntegral index, value)
 
-- Turn the parsed terms and initial values into a 'LinearRecurrence',
-- reporting any mismatch between them.
buildRecurrence :: [(Int, Rational)] -> [(Int, Rational)] -> Parser LinearRecurrence
buildRecurrence terms initialValues = do
  let order = maximum (map fst terms)
      requiredIndices = [0 .. order - 1]
      providedIndices = map fst initialValues
 
      coefficientFor offset = sum [c | (o, c) <- terms, o == offset]
      coefficients = map coefficientFor [1 .. order]
 
      missing = [i | i <- requiredIndices, i `notElem` providedIndices]
      unexpected_ = nub [i | i <- providedIndices, i `notElem` requiredIndices]
      duplicated = nub [i | i <- providedIndices, length (filter (== i) providedIndices) > 1]
 
  unless (null duplicated) $
    fail ("given more than once: " ++ describeIndices duplicated)
 
  unless (null missing) $
    fail
      ( "this recurrence has order "
          ++ show order
          ++ ", so it needs initial values "
          ++ describeIndices requiredIndices
          ++ "; missing "
          ++ describeIndices missing
      )
 
  unless (null unexpected_) $
    fail
      ( "this recurrence has order "
          ++ show order
          ++ ", so only "
          ++ describeIndices requiredIndices
          ++ " are used; remove "
          ++ describeIndices unexpected_
      )
 
  let valueAt index = fromMaybe 0 (lookup index initialValues)
      pairs = zip coefficients (map valueAt requiredIndices)
 
  case NonEmpty.nonEmpty pairs of
    Nothing -> fail "a recurrence must refer to at least one earlier term"
    Just nonEmptyPairs -> pure (linearRecurrence nonEmptyPairs)
  where
    describeIndices indices =
      intercalate ", " ["a(" ++ show i ++ ")" | i <- indices]
 


---------------------------------------
-- Query expressions
----------------------------------------
 
-- | Parse an expression combining already-defined generating functions.
--
-- Unlike 'equationExpr' this supports division, and any name at all is
-- accepted here: whether it actually exists is a question for the
-- evaluator, not the parser.
seriesExpr :: Parser SeriesExpr
seriesExpr = makeExprParser seriesTerm seriesOperators
 
seriesTerm :: Parser SeriesExpr
seriesTerm = do
  base <- seriesAtom
  exponents <- many (symbol "^" *> naturalLiteral)
  pure (foldl SeriesPow base exponents)
 
seriesAtom :: Parser SeriesExpr
seriesAtom =
  parens seriesExpr
    <|> (SeriesX <$ variableX)
    <|> (SeriesLit <$> rationalLiteral)
    <|> (SeriesName <$> identifier)
 
seriesOperators :: [[Operator Parser SeriesExpr]]
seriesOperators =
  [ [Prefix (negated <$ symbol "-")],
    [InfixL (SeriesMul <$ symbol "*"), InfixL (SeriesDiv <$ symbol "/")],
    [InfixL (SeriesAdd <$ symbol "+"), InfixL (SeriesSub <$ symbol "-")]
  ]
  where
    negated = SeriesSub (SeriesLit 0)


    
----------------------------------------
-- Commands
----------------------------------------
 
-- | Parse one line of REPL input.
command :: Parser Command
command =
  (Help <$ keyword "help")
    <|> (ListNames <$ keyword "list")
    <|> (ShowName <$> (keyword "show" *> identifier))
    <|> defineCommand
    <|> coeffsCommand
    <|> coeffCommand
    <|> addCommand
    <|> loadCommand
    <|> (Quit <$ (keyword "quit" <|> keyword "exit"))
 
-- @define NAME by recurrence: ...@ or @define NAME as solution of: ...@
defineCommand :: Parser Command
defineCommand = do
  keyword "define"
  name <- identifier
  byRecurrence name <|> asSolutionOf name
  where
    byRecurrence name = do
      keyword "by"
      keyword "recurrence"
      _ <- symbol ":"
      DefineByRecurrence name <$> recurrenceBody
 
    asSolutionOf name = do
      keyword "as"
      keyword "solution"
      keyword "of"
      _ <- symbol ":"
      specificName name
      _ <- symbol "="
      DefineByEquation name <$> equationExpr name
 
-- @coeffs EXPR N@
--
-- The count can follow the expression without ambiguity precisely because
-- multiplication must be written explicitly: a bare number can never
-- continue an expression, so the expression parser stops of its own accord
-- when it reaches the count.
coeffsCommand :: Parser Command
coeffsCommand = do
  keyword "coeffs"
  expression <- seriesExpr
  count_ <- naturalLiteral
  pure (Coeffs expression (fromIntegral count_))
 
-- @coeff EXPR N@
coeffCommand :: Parser Command
coeffCommand = do
  keyword "coeff"
  expression <- seriesExpr
  index <- naturalLiteral
  pure (CoeffAt expression (fromIntegral index))
 
-- @add A B@, sugar for @coeffs (A + B) 10@.
--
-- The two operands are atoms rather than full expressions, so that
-- @add a b@ cannot be read as @add (a b)@ with a missing second operand.
addCommand :: Parser Command
addCommand = do
  keyword "add"
  left <- seriesAtom
  right <- seriesAtom
  pure (Coeffs (SeriesAdd left right) 10)
 
-- @load PATH@, where the path may be quoted if it contains spaces.
loadCommand :: Parser Command
loadCommand = do
  keyword "load"
  Load <$> (quotedPath <|> barePath)
  where
    quotedPath = lexeme (char '"' *> manyTill L.charLiteral (char '"'))
    barePath = lexeme (some (satisfy (not . isSpace))) <?> "a file path"
 

 
------------------------
-- Running a parser
--------------------------
 
-- Run a parser over a whole input string, requiring it to consume
-- everything, and render any failure with megaparsec's formatting.
runWholeInput :: Parser a -> String -> Either String a
runWholeInput parser input =
  case parse (spaceConsumer *> parser <* eof) "" input of
    Left errorBundle -> Left (errorBundlePretty errorBundle)
    Right result -> Right result
 
-- | Parse one line of REPL input.
--
-- >>> parseCommand "coeffs fib 10"
-- Right (Coeffs (SeriesName "fib") 10)
--
-- >>> parseCommand "add fib catalan"
-- Right (Coeffs (SeriesAdd (SeriesName "fib") (SeriesName "catalan")) 10)
parseCommand :: String -> Either String Command
parseCommand = runWholeInput command
 
-- | Parse the right-hand side of an equation definition, given the name
-- being defined.
--
-- >>> parseEquationRhs "T" "1 + x*T^2"
-- Right (Add (Lit (1 % 1)) (Mul X (Pow Y 2)))
parseEquationRhs :: String -> String -> Either String Expr
parseEquationRhs unknownName = runWholeInput (equationExpr unknownName)
 
-- | Parse a query expression over already-defined names.
--
-- >>> parseSeriesExpr "catalan + fibonacci"
-- Right (SeriesAdd (SeriesName "catalan") (SeriesName "fibonacci"))
parseSeriesExpr :: String -> Either String SeriesExpr
parseSeriesExpr = runWholeInput seriesExpr
 
-- | Parse a recurrence body on its own, without the surrounding @define@.
parseRecurrenceBody :: String -> Either String LinearRecurrence
parseRecurrenceBody = runWholeInput recurrenceBody
 