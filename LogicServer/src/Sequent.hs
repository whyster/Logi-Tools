{-# LANGUAGE LambdaCase #-}
module Sequent (Expr(..), LogicTree(..), Sequent, simplifyTree, solve) where

data Expr = Atom String
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | If Expr Expr
          deriving (Eq, Show)

type Sequent = ([Expr], [Expr])

data LogicTree = Leaf Sequent
               | Branch LogicTree LogicTree
               | Axiom
               | Unsatisfiable
               deriving (Eq, Show)


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
