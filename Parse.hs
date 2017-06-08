module Parse where
import Text.ParserCombinators.Parsec
import System.Environment
import Analyzer
import ArithmeticExpression
import Expressions
import Data.Set as Set
import Data.Map.Strict as Map

readExpr :: String -> String
readExpr input =
  case parse parseProgram "Oexp" input of
    Left err -> "Error"
    Right value -> show (analyze value Map.empty Map.empty)

parseProgram :: Parser Program
parseProgram = parseStatement >>= \statement1 -> parseProgram >>= \program2 ->
  return (Program statement1 program2)

parseExpr = try (parseParenthesized >>= \x -> operation >>= \y -> parseExpr >>= \z ->
              return (ArithmeticExpression x (toOperator y) (checkIfMinus y z)))  <|>
            parseParenthesized <|>
            try (char '-' >> parseParenthesized >>= \x ->
              return (ArithmeticExpression (Singleton Minus) Otimes x)) <|>
            try (parseSingleton >>= \x -> operation >>= \y -> parseExpr >>= \z ->
              return (ArithmeticExpression x (toOperator y) (checkIfMinus y z))) <|>
            try (char '-' >> parseNegativeSingleton >>= \x -> operation >>=
                  \y -> parseExpr >>= \z ->
                    return (ArithmeticExpression x (toOperator y) (checkIfMinus y z)))
            <|> parseSingleton <|>
             try (char '-' >> parseNegativeSingleton)

operation :: Parser Char
operation = oneOf "+-*/"

checkIfMinus :: Char -> ArithmeticExpression -> ArithmeticExpression
checkIfMinus op (ArithmeticExpression singleton opr exp) =
  if singleton == Singleton Zero || elem op "+*/"
    then (ArithmeticExpression singleton opr exp) 
  else
    ArithmeticExpression (Singleton Minus) opr exp
checkIfMinus op (Singleton sign) =
  if sign == Zero || elem op "+*/"
    then Singleton sign
  else
    Singleton Minus

-- SHOULD always be valid because it's called from operation.
toOperator :: Char -> Operator
toOperator op =
  case elem op "*/" of
    True -> Otimes
    _ -> Oplus

toSign :: Integer -> Sign
toSign int =
 if int == 0
   then Zero
 else if int < 0
   then Minus
 else
   Plus

parseSingletonAuxiliary :: (Integer -> Integer) -> Parser ArithmeticExpression
parseSingletonAuxiliary f = many1 digit >>= (return . Singleton . toSign . f . read)

parseSingleton :: Parser ArithmeticExpression
parseSingleton = parseSingletonAuxiliary id

parseNegativeSingleton :: Parser ArithmeticExpression
parseNegativeSingleton = parseSingletonAuxiliary (\x -> -x)
                         

parseParenthesized :: Parser ArithmeticExpression
parseParenthesized = char '(' >>
                     parseExpr >>= \x ->
                     char ')' >>
                     return x

parseStatement :: Parser Statement
parseStatement = parseLabel <|> parseGoto <|> parseVarDec <|> parseIfGoto <|> void

void :: Parser Statement
void = return (Void)

parseLabel :: Parser Statement
parseLabel = (manyTill (char ':') alphaNum) >>= \name ->
                        parseExpression >>= \exp ->
                        return (ExpLabel (Label name) exp)

parseGoto :: Parser Statement
parseGoto = string "goto" >> many1 alphaNum >>= \name -> return (Goto name)

parseVarDec :: Parser Statement
parseVarDec =
  (manyTill (char ' ') alphaNum) >>= \name  ->
  string ":=" >>
  parseExpression >>= \value ->
  return (Define name value)

parseIfGoto :: Parser Statement
parseIfGoto =
  string "if" >>
  parseExpression >>= \exp ->
  string "goto" >>
  manyTill (char ';') alphaNum >>= \name ->
  return (If exp (Label name))

parseExpression :: Parser Expression
parseExpression = parseArithmeticExpression <|>  parseEqualityCheck <|> parseLiteral
  <|> parseVariable

parseArithmeticExpression :: Parser Expression
parseArithmeticExpression =
  parseExpr >>= \exp -> return (Exp exp)

parseEqualityCheck :: Parser Expression
parseEqualityCheck =
  parseArithmeticExpression >>= \exp1 ->
  char '=' >>
  parseArithmeticExpression >>= \exp2 ->
  return (Equal exp1 exp2)

parseLiteral :: Parser Expression
parseLiteral = parseExpr >>= \exp -> return (ExpLit (Literal exp))

parseVariable :: Parser Expression
parseVariable = many1 alphaNum >>= \name -> return (ExpVar name)
