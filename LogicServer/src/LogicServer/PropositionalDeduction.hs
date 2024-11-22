module LogicServer.PropositionalDeduction(structuralEquivilance) where
import LogicServer.Sequent(Expr(..))


data MatchError = Unmatchable

structuralEquivilance :: Expr -> Expr -> Either MatchError [(Expr, Expr)]
structuralEquivilance a@(Atom _) b = Right [(a, b)]
structuralEquivilance (Not e1) (Not e2) = structuralEquivilance e1 e2
structuralEquivilance (If a1 a2) (If b1 b2) = (++) <$> structuralEquivilance a1 b1 <*> structuralEquivilance a2 b2
structuralEquivilance _ _ = Left Unmatchable

type Subst = (Expr, Expr)
substitute :: [Subst] -> Expr -> Expr
substitute subs e = foldl helper e subs
  where helper :: Expr -> Subst -> Expr
        helper e (l, r)
          | e == l = r
        -- helper a@(Atom _) _ = a
        helper (If e1 e2) sub = If (helper e1 sub) (helper e2 sub)
        helper (Not e) sub = Not (helper e sub)
        helper (And xs) sub = And (map (`helper` sub) xs)
        helper (Or xs) sub = Or (map (`helper` sub) xs)
        helper e _ = e


data InferenceRule = Rule
  { given :: [Node]
  , goal :: Node
  , name :: String
  }




data Node = Node
  { formula :: Expr
  , assumptions :: [Node]
  }
