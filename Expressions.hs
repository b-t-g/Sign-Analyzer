module Expressions where
import ArithmeticExpression
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype IntState = IntState (Map.Map (Expressions.Label) (Expressions.Statement))

-- TODO This looks a heck of a lot like arithmetic expression; refactor both to a common
-- structure.
data Program = Program Statement Program | Single Statement

newtype Label = Label String

data Expression = Exp ArithmeticExpression | Equal Expression Expression
                | ExpLit Literal | ExpVar String

-- Matt Might's example only uses integers
newtype Literal = Literal { val :: ArithmeticExpression}

data Variable = Variable String Literal

data Statement = ExpLabel Label Expression | Goto String | Define String Expression
               | If Expression Label | Void
