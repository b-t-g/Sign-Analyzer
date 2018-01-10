module Analyzer where
import ArithmeticExpression
import Expressions
import Data.Set as Set
import Data.Map.Strict as Map
import Data.Maybe as Maybe
type Environment = Map.Map String (Set Sign)
type Labels = Map.Map String Label

preProcess :: Program -> Labels -> Labels
preProcess (Program (ExpLabel (LabelName l) statement) prog) labels =
  preProcess prog (Map.insert l statement labels)
preProcess (Single (Statement (SingleLabel Void))) labels =
  labels
preProcess (Program x y) labels = preProcess y labels



analyze :: Program -> Labels -> Environment -> Environment
analyze (Program statement prog) labels env =
  case statement of
    ExpLabel (LabelName l) statement -> analyze prog labels env
    Statement x                      -> let newEnv = analyzeLabels x labels env in
                                          analyze prog labels newEnv
-- If we only have one statement, pretend it is a program with two statements and reuse
-- the logic above
analyze (Single (Statement (SingleLabel Void))) labels env = env
analyze (Single statement) labels env =
  analyze (Program statement (Single (Statement (SingleLabel Void)))) labels env

analyzeLabels :: Label -> Labels -> Environment -> Environment
analyzeLabels (Label (Goto label) labelStatement) labels env =
     case Map.lookup label labels of
        Just corresponding -> analyzeLabels corresponding labels env
        Nothing            -> env
analyzeLabels (Label (Define varName x) labelStatement) labels env =
  let newEnv = Map.insert varName (analyzeExpression x env) env in
                          analyzeLabels labelStatement labels newEnv
analyzeLabels (Label (If exp label@(LabelName name)) labelStatement) labels env =
      let expr = analyzeExpression exp env in
        if Set.member Plus expr
        then
          case Map.lookup name labels of
            Just corresponding -> analyzeLabels corresponding labels env
            Nothing            -> env
        else
          analyzeLabels labelStatement labels env
analyzeLabels (Label Void labelStatement) labels env = analyzeLabels labelStatement labels env

analyzeLabels (SingleLabel Void) labels env = env
analyzeLabels (SingleLabel statement) labels env =
  analyzeLabels (Label statement (SingleLabel Void)) labels env

analyzeExpression :: Expression -> Environment -> Set Sign
analyzeExpression (Expressions.Exp expr) env =
  abstractAnalyzer expr Set.empty env
analyzeExpression (ExpLit lit) env = abstractAnalyzer (val lit) Set.empty env
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

abstractAnalyzer :: ArithmeticExpression -> Set Sign -> Environment -> Set Sign
abstractAnalyzer (ArithmeticExpression exp1 op exp2) set env =
  let set1 = abstractAnalyzer exp1 set env in
    let set2 = abstractAnalyzer exp2 set env in
      analyzeArithmeticExpression set1 set2 op
abstractAnalyzer (Singleton sign) set env =
  Set.union set (Set.singleton sign)
abstractAnalyzer (Var s) set env =
  Maybe.fromMaybe set (Map.lookup s env)

analyzeArithmeticExpression :: Set Sign -> Set Sign -> Operator -> Set Sign
analyzeArithmeticExpression set1 set2 op =
  Set.foldr (\sign1 -> Set.union (mapOverSet op sign1 set2)) Set.empty set1

mapOverSet :: Operator -> Sign -> Set Sign -> Set Sign
mapOverSet op sign set =
  let setOfSets = Set.map (Analyzer.lookup op sign) set in
    Set.foldr Set.union Set.empty setOfSets

lookup :: Operator -> Sign -> Sign -> Set Sign
lookup Oplus x y
  | x == y = Set.singleton x
lookup Oplus x Zero    = Set.singleton x
lookup Oplus Plus Minus   = Set.union (Set.union (Set.singleton Plus) (Set.singleton Zero)) (Set.singleton Minus)
lookup Otimes x Zero      = Set.singleton Zero
lookup Otimes Plus Minus  = Set.singleton Minus
lookup Otimes x y
  | x == y = Set.singleton Plus
lookup op x y             = Analyzer.lookup op y x
