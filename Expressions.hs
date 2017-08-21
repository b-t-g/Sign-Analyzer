module Expressions where
import ArithmeticExpression
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- I forget what this was supposed to be for...
newtype IntState = IntState (Map.Map (Expressions.Label) (Expressions.Statement))

-- TODO This looks a heck of a lot like arithmetic expression; refactor both to a common
-- structure.
data Program = Program Statement Program | Single Statement
               deriving Show

newtype Label = Label String
              deriving Show

data Expression = Exp ArithmeticExpression | Equal Expression Expression
                | ExpLit Literal | ExpVar String
               deriving Show

-- Matt Might's example only uses integers
newtype Literal = Literal { val :: ArithmeticExpression}
               deriving Show

data Variable = Variable String Literal
               deriving Show

data Statement = ExpLabel Label Statement | Goto String | Define String Expression
               | If Expression Label | Void
               deriving Show
