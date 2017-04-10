module CPi.ParserNew where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as LEX
import Control.Monad
import Data.Hashable (hash)
import Data.Maybe

import CPi.AST hiding ((<|>))

spaceConsumer :: Parser ()
spaceConsumer = LEX.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = LEX.skipLineComment "--"
        blockCmnt = LEX.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = LEX.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = LEX.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

semi :: Parser String
semi = symbol ";"

comma :: Parser String
comma = symbol ","

at :: Parser String
at = symbol "@"

arrow :: Parser String
arrow = symbol "->" <|> symbol "."

reservedWord :: String -> Parser ()
reservedWord w = string w *> notFollowedBy alphaNumChar *> spaceConsumer

reservedWords :: [String]
reservedWords = ["new", "species", "network", "process", "ratelaw", "0"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where p       = (:) <$> lowerChar <*> many alphaNumChar
        check x = if x `elem` reservedWords
                  then fail $ "name " ++ show x
                                      ++ " cannot be used, as it is reserved"
                  else return x

locationName :: Parser String
locationName = (lexeme . try) (p >>= check)
  where p       = (:) <$> (numberChar <|> lowerChar) <*> many alphaNumChar
        check x = if x `elem` reservedWords && x /= "0"
                  then fail $ "name " ++ show x
                                      ++ " cannot be used, as it is reserved"
                  else return x

speciesName :: Parser String
speciesName = (lexeme . try) (p >>= check)
  where p       = (:) <$> upperChar <*> many alphaNumChar
        check x = if x `elem` reservedWords
                  then fail $ "name " ++ show x
                                      ++ " cannot be used, as it is reserved"
                  else return x

number :: Parser Double
number =  lexeme LEX.float

prefix :: Parser Prefix
prefix = lexeme $ do
  name <- identifier
  location <- optional $ do
    void at
    locationName
  return $ case location of
    Just loc -> Located name (fromIntegral $ hash loc)
    Nothing  -> Unlocated name

nil :: Parser Species
nil = const Nil <$> symbol "0"

prefixGuard :: Parser (Prefix, Abstraction)
prefixGuard = do
  pref <- prefix
  abst <- optional $ parens locationName
  sp <- optional $ do
    void $ symbol "->"
    abst' <- optional $ try $ parens locationName
    spec <- atom
    let abst'' = if isNothing abst then abst' else abst
    if isJust abst && isJust abst'
    then fail "trying to bind both before and after guard"
    else return (abst'', spec)
  -- let spec = fromMaybe Nil sp
  return (pref, case sp of
    Just (abst', spec) -> case abst' of
                            Just l  -> mkAbs (fromIntegral $ hash l) spec
                            Nothing -> mkAbsBase spec
    Nothing            -> case abst of
                            Just l  -> mkAbs (fromIntegral $ hash l) Nil
                            Nothing -> mkAbsBase Nil)

guardedSum :: Parser Species
guardedSum = do
  prefSpecs <- prefixGuard `sepBy1` symbol "+"
  return $ mkSum prefSpecs

guardedSum1 :: Parser Species
guardedSum1 = do
  prefSpec <- prefixGuard
  return $ mkSum [prefSpec]

def :: Parser Species
def = do
  name <- speciesName
  appl <- optional $ parens $ do
    args <- identifier `sepBy` comma
    _ <- semi
    locs <- locationName `sepBy` comma
    return (args, locs)
  return $ case appl of
             Just (args, locs) -> Def name args $ map (fromIntegral . hash) locs
             Nothing           -> Def name [] []

atom :: Parser Species
atom = nil <|> def <|> parens species <|> guardedSum1 <|> emptyPar <|> emptySum

term :: Parser Species
term = guardedSum <|> atom <|> restrictionSimple

restrictionSimple :: Parser Species
restrictionSimple = do
  _ <- symbol "new"
  locs <- locationName `sepBy1` comma
  _ <- symbol "in"
  spec <- term
  return $ mkNew (map (fromIntegral . hash) locs) spec

restriction :: Parser Species
restriction = do
  _ <- symbol "new"
  locs <- locationName `sepBy1` comma
  _ <- symbol "in"
  spec <- species
  return $ mkNew (map (fromIntegral . hash) locs) spec

emptyPar :: Parser Species
emptyPar = do
  _ <- symbol "<Empty Par>"
  return $ mkPar []

emptySum :: Parser Species
emptySum = do
  _ <- symbol "<Empty Sum>"
  return $ mkSum []

parallel :: Parser Species
parallel = do
  specs <- term `sepBy1` symbol "|"
  return $ case specs of
             [spec] -> spec
             _      -> mkPar specs

species :: Parser Species
species = restriction <|> parallel

-- abstraction ::

-- guardedSum ::
