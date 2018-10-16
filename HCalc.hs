import Text.Parsec (parse, letter, alphaNum, oneOf, eof, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Token ( whiteSpace
                         , identStart
                         , identLetter
                         , opStart
                         , opLetter
                         , TokenParser
                         , makeTokenParser
                         , naturalOrFloat
                         , identifier
                         , reservedOp
                         , parens)
import Text.Parsec.Expr ( Assoc(AssocLeft)
                        , Assoc(AssocRight)
                        , Operator(Infix)
                        , Operator(Postfix)
                        , Operator(Prefix)
                        , buildExpressionParser )
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (char, try)
import qualified Data.Map as M
import qualified Control.Monad.State as S
import Control.Monad.Error
import Control.Monad.Identity
import System.Console.Haskeline

-- import qualified Numeric.Decimal as N
-- import qualified Numeric.Decimal.Operation as N

type Number = Rational -- N.ExtendedDecimal N.P2000

-- Lexer

def = emptyDef { identStart  = letter
               , identLetter = alphaNum
               , opStart     = oneOf "!+-*/=%^"
               , opLetter    = oneOf "!+-*/=%^"
               }

lexer :: TokenParser ()
lexer = makeTokenParser def

-- Expression tree

data Expression = Constant Number
                | Identifier String
                | Function String Expression
                | Fakultaet Expression
                | Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                | Power Expression Expression
                | Modulo Expression Expression
                | Negation Expression
                | Assignment Expression Expression
                deriving Show

-- data Identifier = Variable String
--                 | Function String Expression

-- Parser

parseNumber :: Parser Expression
parseNumber = do
    v <- naturalOrFloat lexer
    case v of
        Left  i -> return $ Constant $ fromIntegral i
        Right n -> return (Constant (read (show n) :: Number))

parseIdentifier :: Parser Expression
parseIdentifier = do
   i <- identifier lexer
   return $ Identifier i

parseFunction :: Parser Expression
parseFunction = do
  i <- identifier lexer
  arg <- parens lexer parseExpression
  -- char '('
  -- arg <- parseExpression --lexer
  -- char ')'
  return $ Function i arg

parseExpression :: Parser Expression
parseExpression = (flip buildExpressionParser) parseTerm [
   [ Prefix (reservedOp lexer "-" >> return Negation)
   , Prefix (reservedOp lexer "+" >> return id)
   ]
 , [ Postfix (reservedOp lexer "!" >> return Fakultaet)
   ]
 , [
     Infix (reservedOp lexer "^" >> return Power) AssocLeft
   ]
 , [ Infix (reservedOp lexer "*" >> return Multiplication) AssocLeft
   , Infix (reservedOp lexer "/" >> return Division) AssocLeft
   , Infix (reservedOp lexer "%" >> return Modulo) AssocLeft
   ]
 , [ Infix (reservedOp lexer "+" >> return Addition) AssocLeft
   , Infix (reservedOp lexer "-" >> return Subtraction) AssocLeft
   ]
 , [ Infix (reservedOp lexer "=" >> return Assignment) AssocRight
   ]
 ]

parseTerm :: Parser Expression
parseTerm = parens lexer parseExpression
        <|> parseNumber
        <|> do
  try parseFunction <|> parseIdentifier

parseInput :: Parser Expression
parseInput = do
    whiteSpace lexer
    ex <- parseExpression
    eof
    return ex

-- Evaluator

-- abs :: Number -> Number
-- abs x | x >= 0 = x
--       | otherwise = -x

fac :: Number -> Number
fac 0 = 1
fac n = n * fac (n - 1)

decimalMod  :: Number -> Number -> Number
decimalMod x y
  | z >= 0 = decimalMod z y
  | otherwise = x
  where z = x - y

type VarTab = M.Map String Number
type FuncTab = M.Map String (Number -> Number)
type SymTab = (VarTab, FuncTab)

type Evaluator a = S.StateT SymTab (ErrorT String Identity) a

runEvaluator :: Evaluator Number -> SymTab -> Either String (Number, SymTab)
runEvaluator calc tab = runIdentity $ runErrorT $ S.runStateT calc tab

eval :: Expression -> Evaluator Number

eval (Constant x) = return x

eval (Identifier i) = do
  (varTab, _) <- S.get
  case M.lookup i varTab of
    Nothing -> fail $ "Undefined variable " ++ i
    Just e  -> return e

eval (Function i e) = do
  (_, funcTab) <- S.get
  e' <- eval e
  case M.lookup i funcTab of
    Nothing -> fail $ "Undefined function " ++ i
    Just f -> return $ f e'

eval (Fakultaet e) = do
  val <- eval e
  return $ fac val

eval (Power eb ee) = do
  b <- eval eb
  e <- eval ee
  return $ b ^^ round e

eval (Addition eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ lft + rgt

eval (Subtraction eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ lft - rgt

eval (Multiplication eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ lft * rgt

eval (Division eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ lft / rgt

eval (Modulo eLeft eRight) = do
    lft <- eval eLeft
    rgt <- eval eRight
    return $ fromInteger $ floor lft `mod` floor rgt

eval (Negation e) = do
    val <- eval e
    return $ -val

eval (Assignment (Identifier i) e) = do
  val <- eval e
  S.modify (insertVar i val)
  return val

eval (Assignment _ _) =
     fail "Left of assignment must be an identifier"

insertVar :: String -> Number -> SymTab -> SymTab
insertVar i val (varTab, funcTab) =
  let symTab' = M.insert i val varTab
  in (symTab', funcTab)

defaultSyms :: SymTab
defaultSyms = (,)
  (M.fromList
  [ ("e", 1)
  , ("pi", 355/133)
  ])
  (M.fromList
  [ ("sin", toRational . sin . fromRational)
  , ("cos", toRational . cos . fromRational)
  , ("tan", toRational . tan . fromRational)
  , ("exp", toRational . exp . fromRational)
  , ("sqrt", toRational . sqrt . fromRational)
  , ("abs", abs)
  , ("fac", fac)
  ])

--runEvaluator returns Either String (Double, SymTab Double)

calculate :: SymTab -> String -> (String, SymTab)
calculate symTab s =
    case parse parseInput "" s of
    Left  err -> ("error: " ++ (show err), symTab)
    Right exp -> case runEvaluator (eval exp) symTab of
                 Left  err              -> ("error: " ++ err, symTab)
                 Right (val, newSymTab) -> (show val, newSymTab)

loop :: SymTab -> InputT IO ()
loop symTab = do
    line <- getInputLine "> "
    case line of
      Nothing -> return ()
      Just ":q" -> return ()
      Just ":h" -> outputStrLn "This is the help."
      Just input -> do
        let (result, symTab')= calculate symTab input
            symTab'' = insertVar "result" (read result) symTab'
        outputStrLn $ prettyOutput result
        loop symTab''

prettyOutput :: String -> String
prettyOutput = map (\c -> if c == '%' then '/' else c)

main :: IO ()
main = do
  putStrLn "For help type :h"
  runInputT defaultSettings $ loop defaultSyms

-- show
-- Enter expressions, one per line. Empty line to quit --
