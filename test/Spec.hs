-- file test/Spec.hs

import Control.Exception (evaluate)
import Relude
import Test.Hspec
import Test.QuickCheck
import WikiMusic.Test.Head

main :: IO ()
main = hspec $ do
  headSpec
