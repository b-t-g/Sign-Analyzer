module Parse where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import System.Environment
import Analyzer
import ArithmeticExpression
import Expressions
import Data.Set as Set
import Data.Map.Strict as Map

readExpr :: String -> String
readExpr input =
  case parse parseProgram "Oexp" input of
    Left err -> mconcat (Prelude.map (\x -> messageString x ++ "\n") $ errorMessages err)
    Right value -> show (analyze value (preProcess value Map.empty) Map.empty)

parseProgram :: Parser Program
parseProgram = parseStatement >>= \statement1 ->
  case statement1 of
    Statement (SingleLabel Void) -> return (Single (Statement (SingleLabel Void)))
    _    -> parseProgram >>= \program -> return (Program statement1 program)

parseStatement :: Parser Statement
parseStatement = try parseLabel <|>
                 (try parseLabelStatement >>=
                 \labelStatement -> return (Statement (SingleLabel labelStatement)))

parseLabel :: Parser Statement
parseLabel =
  manyTill alphaNum (string ":\n") >>= \name ->
  parseLabelContents >>= \label -> return (ExpLabel (LabelName name) label)

parseLabelContents :: Parser Label
parseLabelContents =
  try parseLabelStatement >>= \statement ->
  case statement of
    Void -> return (SingleLabel Void)
    _    -> (try parseLabelContents >>= \label -> return (Label statement label)) <|>
            return (Label statement (SingleLabel Void))

parseLabelStatement :: Parser LabelStatement
parseLabelStatement = try parseVarDec <|> parseGoto <|> parseIfGoto <|> void

parseVarDec :: Parser LabelStatement
parseVarDec =
  manyTill alphaNum (char ' ') >>= \name  ->
  string ":= " >>
  parseExpression >>= \value ->
  string ";" >>
  optional ( char '\n') >>
  return (Define name value)

parseGoto :: Parser LabelStatement
parseGoto =
  string "goto " >>
  manyTill alphaNum (char ';') >>= \name ->
  optional (char '\n') >>
  return (Goto name)

parseIfGoto :: Parser LabelStatement
parseIfGoto =
  string "if " >>
  parseExpression >>= \exp ->
  string " goto " >>
  manyTill alphaNum (char ';') >>= \name ->
  return (If exp (LabelName name))

void :: Parser LabelStatement
void = eof >> return Void

parseLiteralExpr = try (parseParenthesized >>= \x -> operation >>= \y -> parseLiteralExpr >>= \z ->
              return (ArithmeticExpression x (toOperator y) (checkIfMinus y z)))  <|>
            parseParenthesized <|>
            try (char '-' >> parseParenthesized >>= \x ->
              return (ArithmeticExpression (Singleton Minus) Otimes x)) <|>
            try (parseSingleton >>= \x -> operation >>= \y -> parseLiteralExpr >>= \z ->
              return (ArithmeticExpression x (toOperator y) (checkIfMinus y z))) <|>
            try (char '-' >> parseNegativeSingleton >>= \x -> operation >>=
                  \y -> parseLiteralExpr >>= \z ->
                    return (ArithmeticExpression x (toOperator y) (checkIfMinus y z)))
            <|> parseSingleton <|>
             try (char '-' >> parseNegativeSingleton)

parseSingletonAuxiliary :: (Integer -> Integer) -> Parser ArithmeticExpression
parseSingletonAuxiliary f =  (Singleton . toSign . f . read) <$> many1 digit <|>
                            Var <$> many1 alphaNum

parseSingleton :: Parser ArithmeticExpression
parseSingleton = parseSingletonAuxiliary id

parseNegativeSingleton :: Parser ArithmeticExpression
parseNegativeSingleton = parseSingletonAuxiliary (\x -> -x)

parseParenthesized :: Parser ArithmeticExpression
parseParenthesized = char '(' >>
                     parseLiteralExpr >>= \x ->
                     char ')' >>
                     return x

-- This currently fails for things like Bar := Var+6 (but just Bar := Var works
-- make many1 alphaNum as well as parsing numbers work?)
parseExpression :: Parser Expression
parseExpression = parseArithmeticExpression <|>  parseEqualityCheck <|> parseLiteral
  <|> parseVariable

parseArithmeticExpression :: Parser Expression
parseArithmeticExpression =
  parseLiteralExpr >>= \exp -> return (Exp exp)

parseEqualityCheck :: Parser Expression
parseEqualityCheck =
  parseArithmeticExpression >>= \exp1 ->
  char '=' >>
  parseArithmeticExpression >>= \exp2 ->
  return (Equal exp1 exp2)

parseLiteral :: Parser Expression
parseLiteral = parseLiteralExpr >>= \exp -> return (ExpLit (Literal exp))

parseVariable :: Parser Expression
parseVariable = many1 alphaNum >>= \name -> return (ExpVar name)

operation :: Parser Char
operation = oneOf "+-*/"

checkIfMinus :: Char -> ArithmeticExpression -> ArithmeticExpression
checkIfMinus op (ArithmeticExpression singleton opr exp) =
  if singleton == Singleton Zero || elem op "+*/"
    then ArithmeticExpression singleton opr exp
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
  if op `elem` "*/"
     then Otimes
  else
    Oplus

toSign :: Integer -> Sign
toSign int
 | int == 0 = Zero
 | int < 0 = Minus
 | otherwise = Plus
