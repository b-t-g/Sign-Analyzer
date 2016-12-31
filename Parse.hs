module Parse where
import Text.ParserCombinators.Parsec
import System.Environment
import Analyzer
import ArithmeticExpression
import Data.Set as Set

readExpr :: String -> String
readExpr input =
  case parse parseExpr "Oexp" input of
    Left err -> "Error"
    Right value -> show (analyze value empty)

parseExpr :: Parser ArithmeticExpression
parseExpr = parseParenthesized <|> try (char '-' >> parseParenthesized >>= \x ->
                 return (ArithmeticExpression (Singleton Minus) Otimes x)) <|>
            try (parseSingleton >>= \x -> operation >>= \y -> parseExpr >>= \z ->
                    return (ArithmeticExpression x (toOperator y) (checkIfMinus y z))) <|>
             parseSingleton <|> (char '-' >> parseNegativeSingleton)

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
