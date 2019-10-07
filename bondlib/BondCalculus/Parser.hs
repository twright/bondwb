module BondCalculus.Parser where

import Data.Foldable
import Text.Megaparsec
import Text.Megaparsec.Char
-- import Text.Megaparsec.Expr
import Control.Monad.Combinators.Expr
-- import Text.Megaparsec.Char.Prim (MonadParsec)
import qualified Text.Megaparsec.Char.Lexer as LEX
import qualified Data.Map as M
import Control.Monad
import Data.Hashable (hash)
import Data.Maybe
import Data.Scientific (toRealFloat)
import Data.Bifunctor
-- import Text.Megaparsec.Error (Dec)
-- import Control.Applicative
import Text.Megaparsec.Debug

import BondCalculus.Base
import BondCalculus.AST hiding ((<|>))
import BondCalculus.Symbolic
import Data.Void

-- We need to define a Parser type synonym in megaparsec >= 6.0 
type Parser = Parsec Void String

parseFile :: ExprConstant a => String -> String -> Either (ParseErrorBundle String Void) (BondCalculusModel a)
parseFile = runParser model

parseFileCombined :: String -> String -> Either (ParseErrorBundle String Void) CombinedModel
parseFileCombined = runParser combinedModel

combinedModel :: Parser CombinedModel
combinedModel = do
    x <- lookAhead model
    y <- model
    return $ CombinedModel x y

-- Species:

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

symbReservedWords :: [String]
symbReservedWords = ["sin", "sinh", "asin", "asinh",
                     "cos", "cosh", "acos", "acosh",
                     "tan", "tanh", "atan", "atanh",
                     "exp", "log", "sign", "abs"]

symbIdentifier :: Parser String
symbIdentifier = (lexeme . try) (p >>= check)
  where p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` symbReservedWords
                  then fail $ "name " ++ show x
                                      ++ " cannot be used, as it is reserved"
                  else return x

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

definitionName :: Parser String
definitionName = (lexeme . try) (p >>= check)
  where p       = (:) <$> upperChar <*> many alphaNumChar
        check x = if x `elem` reservedWords
                  then fail $ "name " ++ show x
                                      ++ " cannot be used, as it is reserved"
                  else return x

number :: (Num a, DoubleExpression a) => Parser a
number = fromRational <$> rational
     <|> uncurry fromRationalEndpoints <$> interval
    -- _ <- lookAhead $ do
    --     _ <- symbol "["
    --     _ <- manyTill (anySingleBut ']') (try intervalSep)
    --     _ <- manyTill (anySingleBut ']') (symbol "]")
    --     return ()
    

rational :: Parser Rational
rational = LEX.signed spaceConsumer $ toRational <$> lexeme LEX.scientific

real :: Parser Double
real = LEX.signed spaceConsumer $ toRealFloat <$> lexeme LEX.scientific

intervalSep :: Parser ()
intervalSep = symbol "," <|> symbol ".." >> return ()

intervalDouble :: Parser (Double, Double)
intervalDouble = do
    _ <- symbol "["
    x <- real
    _ <- symbol "," <|> symbol ".."
    y <- real
    _ <- symbol "]"
    return (x, y)

interval :: Parser (Rational, Rational)
interval = do
    _ <- symbol "["
    x <- rational
    _ <- symbol "," <|> symbol ".."
    y <- rational
    _ <- symbol "]"
    return (x, y)

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
  name <- definitionName
  (args, locs) <- fmap (fromMaybe ([], [])) $ optional $ parens $ do
    args <- identifier `sepBy` comma
    locs <- fmap (map (fromIntegral . hash) . fromMaybe []) <$> optional $ do
      _ <- semi
      locationName `sepBy` comma
    return (args, locs)
  return $ Def name args locs

atom :: Parser Species
atom = nil <|> def <|> parens species <|> guardedSum1 <|> emptyPar <|> emptySum

term :: Parser Species
term = guardedSum <|> atom <|> restrictionSimple

restrictionOver :: Parser Species -> Parser Species
restrictionOver base = do
  _ <- symbol "new"
  locs <- locationName `sepBy1` comma
  _ <- symbol "in"
  spec <- base
  return $ mkNew (map (fromIntegral . hash) locs) spec

restriction :: Parser Species
restriction = restrictionOver species

restrictionSimple :: Parser Species
restrictionSimple = restrictionOver term

emptyPar :: Parser Species
emptyPar = do
  _ <- symbol "<Empty Par>"
  return $ mkPar []

emptySum :: Parser Species
emptySum = do
  _ <- symbol "<Empty Sum>"
  return $ mkSum []

-- a version of sepBy1 which only tries to continue capturing the next term,
-- rather than committing - this prevents the parser from being confused by the
-- difference between || and |
sepBy1' :: MonadParsec e s f => f a -> f b -> f [a]
sepBy1' p sep = go
  where go = try ((:) <$> (p <* sep) <*> go) <|> ((: []) <$> p)

parallel :: Parser Species
parallel = do
  specs <- term `sepBy1'` symbol "|"
  return $ case specs of
             [spec] -> spec
             _      -> mkPar specs

species :: Parser Species
species = restriction <|> parallel

-- Processes:
processComponent :: (DoubleExpression a, Show a) => Parser (a, Species)
processComponent = do
  c <- (try number) <|> (brackets (fromFloat <$> real))
  s <- species
  return (c, s)

affinityNetworkAppl :: DoubleExpression a => Parser (AffinityNetworkSpec a)
affinityNetworkAppl = do
  name <- definitionName
  rates <- optional $ parens $ number `sepBy` comma
  let rates' = fromMaybe [] rates
  return $ AffinityNetworkAppl name rates'

affinityNetworkLiteral :: DoubleExpression a
                       => Parser (AffinityNetworkSpec a)
affinityNetworkLiteral = AffinityNetworkSpec <$> affinityNetworkBody

affinityNetwork :: DoubleExpression a
                => Parser (AffinityNetworkSpec a)
affinityNetwork = affinityNetworkAppl <|> affinityNetworkLiteral

processLiteral :: (DoubleExpression a, Show a) => Parser (AbstractProcess a)
processLiteral = do
  components <- processComponent `sepBy` symbol "||"
  _ <- symbol "with"
  _ <- symbol "network"
  network <- affinityNetwork
  return $ Process network components

-- process :: Parser AbstractProcess
-- process = fold <$> processComponent `sepBy` symbol "||"

process :: (DoubleExpression a, Show a) => Parser (AbstractProcess a)
process = fold <$> processCompositionList

processCompositionList :: (DoubleExpression a, Num a, Show a) => Parser [AbstractProcess a]
processCompositionList = (:[]) <$> try processLiteral
                     <|> foldl1 (++) <$> proc `sepBy1'` symbol "||"
    where proc = wrappedProcess
             <|> (:[]) <$> namedProcess
             <|> (:[]) <$> mkProcess mempty . (:[]) <$> processComponent
             <|> (\x -> [mkProcess x []]) <$> affinityNetwork
            --  <|> processLiteral
          wrappedProcess = do
              _ <- symbol "(" 
              p <- processCompositionList
              _ <- symbol ")"
              return p
          namedProcess = ProcessAppl <$> definitionName


-- Symbolic expressions

symbExpr :: (ExprConstant a) => Parser (Expr (Atom a))
symbExpr = makeExprParser symbTerm symbExprTable
  where symbExprTable = [ [ prefixOp "-" negate
                          , prefixOp "+" id ]
                        , [ binaryOp "**" (**)
                          , binaryOp "^" (**) ]
                        , [ binaryOp "*" (*)
                          , binaryOp "/" (/) ]
                        , [ binaryOp "+" (+)
                          , binaryOp "-" (-) ] ]
        binaryOp name f = InfixL (f <$ symbol name)
        prefixOp name f = Prefix (f <$ symbol name)

symbTerm :: forall a . (ExprConstant a) => Parser (Expr (Atom a))
symbTerm = parens symbExpr
       <|> symbol "sin"   *> (sin    <$> symbTerm)
       <|> symbol "sinh"  *> (sinh   <$> symbTerm)
       <|> symbol "asin"  *> (asin   <$> symbTerm)
       <|> symbol "asinh" *> (asinh  <$> symbTerm)
       <|> symbol "cos"   *> (cos    <$> symbTerm)
       <|> symbol "cosh"  *> (cosh   <$> symbTerm)
       <|> symbol "acos"  *> (acos   <$> symbTerm)
       <|> symbol "acosh" *> (cosh   <$> symbTerm)
       <|> symbol "tan"   *> (tan    <$> symbTerm)
       <|> symbol "tanh"  *> (tanh   <$> symbTerm)
       <|> symbol "atan"  *> (atan   <$> symbTerm)
       <|> symbol "atanh" *> (atanh  <$> symbTerm)
       <|> symbol "exp"   *> (exp    <$> symbTerm)
       <|> symbol "log"   *> (log    <$> symbTerm)
       <|> symbol "abs"   *> (abs    <$> symbTerm)
       <|> symbol "sign"  *> (signum <$> symbTerm)
       <|> (val :: a -> Expr(Atom a)) <$> number
       <|> var <$> symbIdentifier

-- Definitions:
kineticLawDef :: ExprConstant a => Parser (String, KineticLawDefinition a)
kineticLawDef = do
  _ <- symbol "kinetic"
  _ <- symbol "law"
  name <- definitionName
  (params, args) <- parens $ do
    params <- symbIdentifier `sepBy` comma
    _ <- semi
    args <- symbIdentifier `sepBy` comma
    return (params, args)
  _ <- symbol "="
  body <- symbExpr
  _ <- semi
  let unboundVars = filter (`notElem` (params ++ args)) $ freeVars body
  if null unboundVars
  then return (name, KineticLawDef name params args body)
  else fail $ "Definition of " ++ name ++ " which binds "
              ++ show (params ++ args) ++ " contains unbound variables."

concreteKineticLawDef :: ExprConstant a => Parser (String, RateLawFamily a)
concreteKineticLawDef = second concretifyKineticLaw <$> kineticLawDef

processDef :: (DoubleExpression a, Show a) => Parser (String, AbstractProcess a)
processDef = do
  _ <- symbol "process"
  name <- definitionName
  _ <- symbol "="
  p <- process
  _ <- semi
  return (name, p)

siteSep :: Parser String
siteSep = symbol "+" <|> (notFollowedBy (symbol "||") >> symbol "|")

siteList :: Parser [String]
siteList = identifier `sepBy1` siteSep

rateLawParam :: DoubleExpression a => Parser (RateLawParam a)
rateLawParam = variable <|> value
  where variable = fmap RateLawParamVar identifier
        value    = fmap RateLawParamVal number

rateLawAppl :: DoubleExpression a => Parser (RateLawSpec a)
rateLawAppl = do
  name <- definitionName
  params <- optional $ parens $ rateLawParam `sepBy` comma
  let params' = fromMaybe [] params
  return $ RateLawAppl name params'

affinity :: DoubleExpression a => Parser (Affinity a)
affinity = do
  sites <- siteList `sepBy1` (symbol "||" <|> comma)
  _ <- symbol "at"
  _ <- symbol "rate"
  rateLaw <- rateLawAppl
  return $ Affinity rateLaw sites

affinityNetworkDef :: DoubleExpression a
                   => Parser (String, AffinityNetworkDefinition a)
affinityNetworkDef = do
  _ <- symbol "affinity"
  _ <- symbol "network"
  name <- definitionName
  rates <- optional $ parens $ identifier `sepBy` comma
  let rates' = fromMaybe [] rates
  _ <- symbol "="
  affinities <- affinityNetworkBody
  return (name, AffinityNetworkDef rates' affinities)

affinityNetworkBody :: DoubleExpression a
                    => Parser (AffinityNetwork a)
affinityNetworkBody = braces $ many $ do aff <- affinity
                                         _ <- semi
                                         return aff

speciesDef :: Parser (String, SpeciesDefinition)
speciesDef = do
  _ <- symbol "species"
  name <- definitionName
  argSpec <- optional $ parens $ do
    args <- identifier `sepBy` comma
    _ <- semi
    locs <- locationName `sepBy` comma
    return (args, locs)
  let (args, locs) = fromMaybe ([], []) argSpec
      locs' = map (fromIntegral . hash) locs
  _ <- symbol "="
  body <- species
  _ <- semi
  let unboundLocs = filter (`notElem` locs') $ freeLocs body
  if null unboundLocs
  then return (name, SpeciesDef args locs' body)
  else fail $ "Definition of " ++ name ++ " which binds " ++ show locs
              ++ " contains unbound locations."

definition :: ExprConstant a => Parser (BondCalculusModel a)
definition = sDef <|> pDef <|> aDef <|> kDef
  where -- we use a real empty model, rather than the default template as we
        -- will combine with this as unit later
        z = Defs M.empty M.empty M.empty M.empty
        sDef = (\(n,x) -> addSpeciesDef n x z) <$> speciesDef
        pDef = (\(n,x) -> addProcessDef n x z) <$> processDef
        kDef = (\(n,x) -> addKineticLawDef n x z) <$> concreteKineticLawDef
        aDef = (\(n,x) -> addAffinityNetworkDef n x z) <$> affinityNetworkDef

model :: ExprConstant a => Parser (BondCalculusModel a)
model = foldl combineModels emptyBondCalculusModel <$> many definition
