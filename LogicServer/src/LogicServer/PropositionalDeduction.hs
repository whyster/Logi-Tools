module PropositionalDeduction() where 
import LogicServer.Sequent(Expr(..))




data InferenceRule = Rule 
  { given :: [Node]
  , goal :: Node
  , name :: String
  }




data Node = Node 
  { formula :: Expr
  , assumptions :: [Node]
  }
