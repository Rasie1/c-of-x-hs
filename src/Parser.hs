module Parser where

import Control.Monad (void)
import Data.Void
import Data.Bifunctor (first)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import AST
import Data.Text (Text)
import Data.Char (isDigit, isAlphaNum)
import qualified Data.Text as Text

parseExpression :: String -> Either String Expression
parseExpression = first show . runParser file ""

type Parser = Parsec Void String

allSpaceConsumer :: Parser ()
allSpaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

spaceConsumer :: Parser ()
spaceConsumer = L.space (skipSome (char ' ')) (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

line :: Parser Expression
line = do
  ret <- expression
  void eol <|> eof
  return ret

file :: Parser Expression
file = foldThen <$> (some line)
  where foldThen :: [Expression] -> Expression
        foldThen [exp] = exp
        foldThen xs = foldl1 EThen xs

integer :: Parser Integer
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedWords :: [String]
reservedWords = ["in","let","pos"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

expressionParser :: Parser Expression
expressionParser = between spaceConsumer eof expression

expression :: Parser Expression
expression = operators_ folded
  where folded = foldApps <$> some expressionAtom
        foldApps :: [Expression] -> Expression
        foldApps [exp] = exp
        foldApps xs = foldl1 EApp xs


expressionAtom :: Parser Expression
expressionAtom = parens expression 
             <|> let_ 
             <|> literal_ 
             <|> any_
             <|> EVar . Text.pack 
             <$> identifier

operators_ e =
    makeExprParser e
      [
        [
          InfixL $ EMult <$ symbol "*"
        ],
        [
          InfixL $ EAdd <$ symbol "+"
        , InfixL $ ESub <$ symbol "-"
        ],
        [
          InfixR $ EAbs <$ symbol "=>"
        ]
      ]

let_ :: Parser Expression
let_ = do
  rword "let"
  var <- identifier
  void (symbol "be")
  expression1 <- expression
  rword "in"
  expression2 <- expression
  pure (ELet (Text.pack var) expression1 expression2)

literal_ = ELit <$> (
  LInt <$> integer
  <|> (symbol "True" *> pure (LBool True))
  <|> (symbol "False" *> pure (LBool False))
  )

any_ = symbol "_" *> pure EAny
