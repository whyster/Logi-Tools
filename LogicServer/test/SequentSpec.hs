module SequentSpec(spec) where
import Test.Hspec
import Test.Hspec.QuickCheck
import LogicServer.Sequent(Expr(..), LogicTree(Leaf), bic, solve)
import Data.Aeson(encode, decode)
import Data.Maybe

spec :: Spec
spec = do
  describe "solve" solveSpec
  describe "FromJSON/ToJSON" jsonSpec


jsonSpec :: Spec
jsonSpec = parallel $ do
    prop "decoding an encoded structure returns the same structure" $ do
      let propEquivilantEncoding :: Expr -> Maybe Bool
          propEquivilantEncoding expr = (== expr) <$> (decode . encode $ expr)
      fromJust . propEquivilantEncoding 


solveSpec :: Spec
solveSpec = parallel $ do
    modifyMaxSize (const 32) $ prop "explosion proves all" $ do
      \consequence -> solve (Leaf ([Atom "P", Not $ Atom "P"], [consequence]))
    -- Doing big logic tress cause will take forever 
    modifyMaxSize (const 16) $ prop "proves a sequent is equivilant to implication" $ do
      \expA expB -> solve (Leaf ([], [If expA expB])) == solve (Leaf ([expA], [expB]))
    modifyMaxSize (const 32) $ prop "always terminates" $ do
      \expA expB -> solve (Leaf ([expA], [expB])) `shouldSatisfy` (\x -> x || not x)
    it "proves falsum are unsatisfiable" $ do
      solve (Leaf ([], [Atom "P"])) `shouldBe` False
    it "proves complex falsum are unsatisfiable" $ do
      solve (Leaf ([], [And [(Or [(Atom "Q"), (Atom "P")]), (Or [(Atom "G"), (Atom "H")])] ])) `shouldBe` False
    it "proves empty sequents are unsatisfiable" $ do
      solve (Leaf ([], [])) `shouldBe` False
    describe "for known tautaulogies" $ do
      it "proves law of excluded middle" $ do
        solve (Leaf ([], [Or [(Atom "A"), (Not $ Atom "A")]]))
      it "proves law of contraposition" $ do
        solve (Leaf ([], [If (Atom "A") (Atom "B") `bic` If (Not $ Atom "B") (Not $ Atom "A")]))
      it "proves reducto ad absurdim" $ do
        solve (Leaf ([And [(If (Not $ Atom "A") (Atom "B")), (If (Not $ Atom "A") (Not $ Atom "B"))] ], [Atom "A"]))
      it "proves negation of tautaulogy is unsatisfiable" $ do
        solve (Leaf ([Not $ And [(If (Not $ Atom "A") (Atom "B")), (If (Not $ Atom "A") (Not $ Atom "B"))] ], [Atom "A"])) `shouldBe` False
