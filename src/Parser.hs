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
parseExpression = first show . runParser expressionParser ""

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- block :: Parser -> Parser a


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
foldApps xs = foldl1 EApp  xs

expressionAtom :: Parser Expression
expressionAtom = parens expression 
             <|> let_ 
             <|> abstraction_ 
             <|> pos_
             <|> literal_ 
             <|> EVar . Text.pack 
             <$> identifier

-- prefixParser =
--   do
--     prefixOps <- many prefixOp
--     exp <- exponentiationParser
--     return $ foldr ($) exp prefixOps
--   where
--     prefixOp = MonOp MonoMinus <$ symbol "-" <|> MonOp MonoPlus <$ symbol "+"


-- exponentiationParser =
--   do
--     lhs <- termParser
--     -- Loop back up to prefix instead of going down to term
--     rhs <- optional (symbol "^" >> prefixParser)
--     return $ maybe lhs (BinOp BinaryExp lhs) rhs

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

pos_ :: Parser Expression
pos_ = do
  pos <- L.indentLevel
  rword "pos"
  pure (EPos pos)

abstraction_ :: Parser Expression
abstraction_ = do
  void (symbol "\\")
  var <- identifier
  (symbol "->")
  ELam (Text.pack var) <$> expression

literal_ = ELit <$> (
  LInt <$> integer
  <|> (symbol "true" *> pure (LBool True))
  <|> (symbol "false" *> pure (LBool False))
  )
