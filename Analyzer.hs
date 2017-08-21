module Analyzer where
import ArithmeticExpression
import Expressions
import Data.Set as Set
import Data.Map.Strict as Map
type Environment = Map.Map (String) (Set Sign)

analyze :: Program -> Environment -> Map.Map (String) (Statement) -> Map.Map (String) (Set Sign)
analyze (Program statement prog) env labels = 
  case statement of
    ExpLabel (Label l) statement    -> analyze prog env (Map.insert l statement labels)
    Goto label -> case Map.lookup label labels of
        Just corresponding -> analyze (Single corresponding) env labels
        Nothing            -> env
    Define varName x -> let newEnv = Map.insert (varName) (analyzeExpression x env) env in
                          analyze prog newEnv labels
    If exp label@(Label name) ->
      let expr = analyzeExpression exp env in
        if Set.member Plus expr
        then 
          case Map.lookup name labels of
            Just corresponding -> analyze (Single corresponding) env labels
            Nothing            -> env
        else
          analyze prog env labels
    Void                      -> env
analyze (Single statement) env labels = env

  -- data Statement = ExpLabel Label Statement | Goto String | Define String Expression
  --             | If Expression Label | Void
analyzeExpression :: Expression -> Environment -> Set Sign
analyzeExpression (Expressions.Exp expr) env =
  abstractAnalyzer expr Set.empty
analyzeExpression (ExpLit lit) env = abstractAnalyzer (val lit) Set.empty
analyzeExpression (Equal exp1 exp2) env =
  let val1 = analyzeExpression exp1 env in
    let val2 = analyzeExpression exp2 env in
      if Set.intersection val1 val2 == Set.empty
        then Set.singleton Minus
        else Set.singleton Plus
analyzeExpression (ExpVar varName) env =
  case Map.lookup varName env of
    Just lit -> lit
    _        -> Set.union (Set.union (Set.singleton Plus) (Set.singleton Zero))
                         (Set.singleton Minus)

abstractAnalyzer :: ArithmeticExpression -> Set Sign -> Set Sign
abstractAnalyzer (ArithmeticExpression exp1 op exp2) set =
  let set1 = abstractAnalyzer exp1 set in
    let set2 =  abstractAnalyzer exp2 set in
      analyzeArithmeticExpression set1 set2 op
abstractAnalyzer (Singleton sign) set =
  Set.union set (Set.singleton sign)

analyzeArithmeticExpression :: Set Sign -> Set Sign -> Operator -> Set Sign
analyzeArithmeticExpression set1 set2 op =
  Set.foldr (\sign1 -> Set.union (mapOverSet op sign1 set2)) Set.empty set1

mapOverSet :: Operator -> Sign -> Set Sign -> Set Sign
mapOverSet op sign set =
  let setOfSets = Set.map (\sign2 -> Analyzer.lookup op sign sign2) set in
    Set.foldr Set.union Set.empty setOfSets

lookup :: Operator -> Sign -> Sign -> Set Sign
lookup Oplus Plus Plus    = Set.singleton Plus
lookup Oplus Minus Minus  = Set.singleton Minus
lookup Oplus Plus Zero    = Set.singleton Plus
lookup Oplus Zero Plus    = Set.singleton Plus
lookup Oplus Minus Zero   = Set.singleton Minus
lookup Oplus Zero Minus   = Set.singleton Minus
lookup Oplus Plus Minus   = Set.union (Set.union (Set.singleton Plus) (Set.singleton Zero)) (Set.singleton Minus)
lookup Oplus Minus Plus   = Set.union (Set.union (Set.singleton Plus) (Set.singleton Zero)) (Set.singleton Minus)
lookup Oplus Zero Zero    = Set.singleton Zero
lookup Otimes Plus Plus   = Set.singleton Plus
lookup Otimes Minus Minus = Set.singleton Plus
lookup Otimes Plus Zero   = Set.singleton Zero
lookup Otimes Zero Plus   = Set.singleton Zero
lookup Otimes Minus Zero  = Set.singleton Zero
lookup Otimes Zero Minus  = Set.singleton Zero
lookup Otimes Plus Minus  = Set.singleton Minus
lookup Otimes Minus Plus  = Set.singleton Minus
lookup Otimes Zero Zero   = Set.singleton Zero
