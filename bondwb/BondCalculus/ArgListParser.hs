module BondCalculus.ArgListParser where

import Text.Megaparsec
import Text.Megaparsec.Char
-- import Text.Megaparsec.Expr
-- import Control.Monad.Combinators.Expr
-- import Text.Megaparsec.Char.Prim (MonadParsec)
import qualified Text.Megaparsec.Char.Lexer as LEX
-- import qualified Data.Map as M
import Control.Monad
-- import Data.Hashable (hash)
import Data.Maybe
import Data.Scientific (toRealFloat)
import Data.Bifunctor
-- import Text.Megaparsec.Error (Dec)
-- import Control.Applicative

import Data.Void
import BondCalculus.Parser (Parser)

-- We need to define a Parser type synonym in megaparsec >= 6.0 
-- type Parser = Parsec Void String

-- parseFile :: String -> String -> Either (ParseErrorBundle String Void) BondCalculusModel
-- parseFile = runParser model

-- Species:

spaceConsumer :: Parser ()
spaceConsumer = LEX.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = LEX.skipLineComment "#"
        blockCmnt = LEX.skipBlockComment "#*" "*#"

lexeme :: Parser a -> Parser a
lexeme = LEX.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = LEX.symbol spaceConsumer

argChar :: Parser Char
argChar = alphaNumChar <|> oneOf "=-/."

arg :: Parser String
arg = (lexeme . try) $ (p >>= return) <|> stringLiteral
  where p = (:) <$> argChar <*> many argChar

args :: Parser [String]
args = many arg

doubleQuote :: Parser Char
doubleQuote = char '"'

stringLiteral :: Parser String
stringLiteral = doubleQuote >> manyTill LEX.charLiteral doubleQuote
