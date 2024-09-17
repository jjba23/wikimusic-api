import Relude
import Test.Hspec
import WikiMusic.Test.Integration

main :: IO ()
main = hspec $ do
  integrationSpec
