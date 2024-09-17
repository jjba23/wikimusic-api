module WikiMusic.Test.Integration where

import Control.Concurrent
import WikiMusic.Test.Principium
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Relude
import Test.Hspec

integrationSpec :: SpecWith ()
integrationSpec =
  describe "starts WikiMusic application properly" $ do
    it "starts and shuts down the application properly" $ do
      res <- runWikiMusic (const . liftIO $ threadDelay 1000000)
      res `shouldBe` Right ()
    it "checks that the wanted port is reachable" $ do
      res <- runWikiMusic (\cfg -> httpCall (mkTestUrl cfg <> "/system-information"))
      second (statusCode . responseStatus) res `shouldBe` Right 200


