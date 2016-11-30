module Analyzer where
import ArithmeticExpression
import Data.Set as Set

analyze :: ArithmeticExpression -> Set Sign -> Set Sign
analyze exp set = 
  case exp of
    (ArithmeticExpression exp1 op exp2) -> let set1 = analyze exp1 set in
                                             let set2 = analyze exp2 set in
                                               analyzeExpression set1 set2 op
    (Singleton sign) -> union set (singleton sign)

analyzeExpression :: Set Sign -> Set Sign -> Operator -> Set Sign
analyzeExpression set1 set2 op =
  Set.foldr (\sign1 -> union (mapOverSet op sign1 set2)) empty set1

mapOverSet :: Operator -> Sign -> Set Sign -> Set Sign
mapOverSet op sign set =
  let setOfSets = Set.map (\sign2 -> Analyzer.lookup op sign sign2) set in
    Set.foldr union empty setOfSets

lookup :: Operator -> Sign -> Sign -> Set Sign
lookup Oplus Plus Plus = singleton Plus
lookup Oplus Minus Minus = singleton Minus
lookup Oplus Plus Zero = singleton Plus
lookup Oplus Zero Plus = singleton Plus
lookup Oplus Minus Zero = singleton Minus
lookup Oplus Zero Minus = singleton Minus
lookup Oplus Plus Minus = union (union (singleton Plus) (singleton Zero)) (singleton Minus)
lookup Oplus Minus Plus = union (union (singleton Plus) (singleton Zero)) (singleton Minus)
lookup Oplus Zero Zero = singleton Zero
lookup Otimes Plus Plus = singleton Plus
lookup Otimes Minus Minus = singleton Plus
lookup Otimes Plus Zero = singleton Zero
lookup Otimes Zero Plus = singleton Zero
lookup Otimes Minus Zero = singleton Zero
lookup Otimes Zero Minus = singleton Zero
lookup Otimes Plus Minus = singleton Minus
lookup Otimes Minus Plus = singleton Minus
lookup Otimes Zero Zero = singleton Zero
