--An LL(1) parser for arithmetic expression, the results of which will be passed to the analyzer where the
--sign will be determined
module Parse where

--Only what the operation does to the sign matters, hence subtraction will be treated as Oplus and
--Division will be treated as Otimes.
data Analyzer = Oplus | Otimes

-- Since parenthesization will be used to determine operation order, we need a way to distinguish
-- a parenthesized expression so we will know what to evaluate first.
data O_expression = O_expression Integer Analyzer O_expression | Parenthesized_o_expression
                  | Singleton Integer

type Parenthesized_o_expression = O_expression

type Parse_error = String

--Used to denote that an o_expression has successfully been parsed, with some unparsed input.
type Partial_o_expression = (O_expression, String)

possible_operations = "+-*/"

--Check for exceptional cases and then delegate to more specialized parsers.
parse :: (Maybe Partial_o_expression) -> Either Parse_error Partial_o_expression 
parse possible_expression expr =
  if expr == "" || expr == []
    then
      case possible_expression of
        Just x -> Right $ fst x
        Nothing -> Left "Tried to parse empty expression"
  else
    if head expr == '('
      then parse_parenthesized $ tail expr
    else
      parse_o_expression expr

{- handle the parenthesized case, it can be a bit tricky since the parenthesized o_expression
 - "belongs" to the - o_expression which precedes it, a bit more analysis needs to be done than
 - usual before emitting an o_expression.
 -}
parse_parenthesized :: String -> Either Parse_error Partial_o_expression
parse_parenthesized expr = 
  case parse_o_expression expr of
    Left error_message -> error_message


parse_o_expression :: String -> Either Parse_error Partial_o_expression
parse_o_expression = undefined

