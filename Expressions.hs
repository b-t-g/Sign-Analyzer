module Expressions where
import ArithmeticExpression
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Program = Program Statement Program | Single Statement
               deriving Show

newtype LabelName = LabelName String
              deriving Show

data Expression = Exp ArithmeticExpression | Equal Expression Expression
                | ExpLit Literal | ExpVar String
               deriving Show

-- Matt Might's example only uses integers
newtype Literal = Literal { val :: ArithmeticExpression}
               deriving Show

data Variable = Variable String Literal
               deriving Show

data Label = Label LabelStatement Label | SingleLabel LabelStatement deriving Show

-- Valid statements in a label
data LabelStatement = Goto String | Define String Expression | If Expression LabelName | Void
                    deriving Show

-- A general statement can be anything that can be done in a label plus defining a label.
data Statement = ExpLabel LabelName Label | Statement Label deriving Show
