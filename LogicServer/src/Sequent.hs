{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Sequent (Expr(..), LogicTree(..), Sequent, simplifyTree, solve, bic, explosionExpr) where
import Test.QuickCheck
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.KeyMap hiding (foldr)

data Expr = Atom String
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | If Expr Expr
          deriving (Eq, Show)

instance ToJSON Expr where
  toJSON :: Expr -> Value
  toJSON (Atom s)    = object [ "tag" .= String "atom", "value" .= s]
  toJSON (Not e)     = object [ "tag" .= String "not", "value" .= toJSON e]
  toJSON (And e1 e2) = object [ "tag" .= String "and", "value" .= toJSONList [e1, e2]]
  toJSON (Or e1 e2)  = object [ "tag" .= String "or", "value" .= toJSONList [e1, e2]]
  toJSON (If e1 e2)  = object [ "tag" .= String "if", "value" .= object ["antecedent" .= toJSON e1, "consequent" .= toJSON e2]]

instance FromJSON Expr where
  parseJSON :: Value -> Parser Expr
  parseJSON = withObject "Coord" $ \v -> case v !? "tag" of
      Just (String "atom") -> Atom <$> (v .: "value")
      Just (String "not")  -> Not  <$> (v .: "value") 
      Just (String "and")  -> (v .: "value") >>= (\(fst : snd : _) -> return (And fst snd))
      Just (String "or")   -> (v .: "value") >>= (\(fst : snd : _) -> return (Or fst snd))
      Just (String "if")   -> (v .: "value") >>= (\o -> If <$> (o .: "antecedent") <*> (o .: "consequent"))
      _ -> fail "Unexpected tag value"


type Sequent = ([Expr], [Expr])

data LogicTree = Leaf Sequent
               | Branch LogicTree LogicTree
               | Axiom
               | Unsatisfiable
               deriving (Eq, Show)

bic :: Expr -> Expr -> Expr
bic a b = And (If a b) (If b a)

instance Arbitrary Expr where
  arbitrary = sized exprs

instance CoArbitrary Expr where
  coarbitrary (Atom s)   = variant 0 . coarbitrary s
  coarbitrary (Not e)    = variant 1 . coarbitrary e
  coarbitrary (And e2 e) = variant 2 . coarbitrary e2 . coarbitrary e
  coarbitrary (Or e2 e)  = variant 3 . coarbitrary e2 . coarbitrary e
  coarbitrary (If e2 e)  = variant 4 . coarbitrary e2 . coarbitrary e



exprs :: Int -> Gen Expr
exprs n
    | n <= 0 = fmap Atom arbitrary
    | otherwise = oneof [ Not <$> subExpr
                        , And <$> subExpr <*> subExpr
                        , Or <$> subExpr <*> subExpr
                        , If <$> subExpr <*> subExpr
                        ]
  where subExpr = exprs $ n `div` 2


simplifySequent :: Sequent -> LogicTree
simplifySequent (left, right) =
  case left of
    ((And a b) : xs) -> Leaf (a : b : xs ,right)
    ((Or a b) : xs)  -> Branch (Leaf (a : xs, right)) (Leaf (b : xs, right))
    ((If a b) : xs)  -> Branch (Leaf (xs, a : right)) (Leaf (b : xs, right))
    ((Not a) : xs)   -> Leaf (xs, a : right)
    []               -> processRight [] right
    (x : xs)         -> if x `elem` right then Axiom else processRight (xs ++ [x]) right
  where processRight :: [Expr] -> [Expr] -> LogicTree
        processRight left' right' = case right' of
          []               -> Leaf (left', [])
          ((And a b) : xs) -> Branch (Leaf (left', a : xs)) (Leaf (left', b : xs))
          ((Or a b) : xs)  -> Leaf (left', a : b : xs)
          ((If a b) : xs)  -> Leaf (a : left', b : xs)
          ((Not a) : xs)   -> Leaf (a : left', xs)
          (x : xs)         -> Leaf (left', xs ++ [x])

isAtom :: Expr -> Bool
isAtom (Atom _) = True
isAtom _        = False

isAllAtoms :: [Expr] -> Bool
isAllAtoms = foldr ((&&) . isAtom) True

isAtomic :: Sequent -> Bool
isAtomic (left, right) = isAllAtoms left && isAllAtoms right

isUnsatisfiable :: Sequent -> Bool
isUnsatisfiable (left, right) = all (`notElem` right) left


simplifyTree :: LogicTree -> LogicTree
simplifyTree Axiom         = Axiom
simplifyTree Unsatisfiable = Unsatisfiable

simplifyTree (Leaf sequent) = case simplifySequent sequent of
  (Leaf sequent')
    | isAtomic sequent' && isUnsatisfiable sequent' -> Unsatisfiable
  rest -> rest

simplifyTree (Branch left right) = case (leftS, rightS) of
    (Axiom, Axiom)     -> Axiom
    (Unsatisfiable, _) -> Unsatisfiable
    (_, Unsatisfiable) -> Unsatisfiable
    (a, b)             -> Branch a b
  where leftS = simplifyTree left
        rightS = simplifyTree right


solve :: LogicTree -> Bool
solve tree = case until terminates simplifyTree tree of
    Axiom -> True
    Unsatisfiable -> False
    _ -> False
  where terminates = \case Axiom -> True
                           Unsatisfiable -> True
                           _ -> False

explosionExpr = If (And (Atom "phi") (Not $ Atom "phi")) (Atom "zeta")



