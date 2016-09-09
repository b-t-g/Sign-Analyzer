--An LL(1) parser for arithmetic expression, the results of which will be passed to the analyzer where the
--sign will be determined
module Parse where

data Analyzer = Oplus | Otimes

-- Since parenthesization will be used to determine operation order, we need a way to distinguish
-- a parenthesized expression so we will know what to evaluate first.
data O_expression = O_expression Integer Analyzer O_expression | Parenthesized_o_expression
                  | Singleton Integer

type Parentheszed_o_expression = O_expression

parse :: [String] -> O_expression
parse expr = 
