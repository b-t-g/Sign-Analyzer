module Parse where

data Analyzer = Oplus | Otimes

data O_expression = O_expression Integer Analyzer O_expression | Parenthesized_o_expression
                  | Singleton Integer

type Parentheszed_o_expression = O_expression

parse :: [String] -> O_expression
parse expr = undefined
