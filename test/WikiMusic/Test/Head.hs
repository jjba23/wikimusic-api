module WikiMusic.Test.Head where
  
import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck
import Relude

headSpec =
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head (23 :| []) `shouldBe` (23 :: Int)

