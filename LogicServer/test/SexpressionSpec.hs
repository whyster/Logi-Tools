module SexpressionSpec where
import Test.Hspec
import LogicServer.Sexpressions (Sexpr(..), sexpParser)
import Text.Parsec
import Data.Maybe (fromJust)
import Control.Monad

spec :: Spec
spec = describe "sexpParser" $ do
  describe "Parses Atoms" $
    forM_ ["example", "EXAMPLE", "\\phi", "\"quoted atom\""] $ \input ->
      it ("Parse \"" ++ input ++ "\"") $ do
        fromJust (parseSexpr input) `shouldBe` Atom (stripQuotes input)
  -- it "Succeeds" $ do
  --   True
  -- it "Parses atoms" $ do
  --   forM_ $
    -- fromJust (parseSexpr input) `shouldBe` Atom input

stripQuotes :: String -> String
stripQuotes = filter (/= '"')

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b)  = Just b

parseSexpr :: String -> Maybe Sexpr
parseSexpr = eitherToMaybe . runParser sexpParser () "(Test)"
