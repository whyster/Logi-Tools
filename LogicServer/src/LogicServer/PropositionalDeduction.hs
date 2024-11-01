module PropositionalDeduction() where 
import LogicServer.Sequent(Expr(..))



data InferenceRule
  = Assumption
  | ConjunctionIntroduction
  | ConjunctionElimination
  | DisjunctionIntroduction
  | DisjunctionElimination
  | InferenceIntroduction
  | InferenceElimination
  | NegationIntroduction
  | NegationElimination
  | BiconditionalIntroduction
  | BiconditionalElimination




data Node = Node 
  { formula :: Expr
  , assumptions :: [Node]
  , inferenceRule :: InferenceRule
  }
