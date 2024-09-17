module WikiMusic.Test.Integration where

import Control.Concurrent
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Relude
import Test.Hspec
import WikiMusic.Model.Config
import WikiMusic.Test.Principium

integrationSpec :: SpecWith ()
integrationSpec =
  describe "starts WikiMusic application properly" $ do
    it "starts and shuts down the application properly" $ do
      res <- runWikiMusic (const . liftIO $ threadDelay 1000000)
      res `shouldBe` Right ()
    it "checks that the system information route is reachable" $ do
      res <- runWikiMusic (\cfg -> httpCall (mkTestUrl cfg <> "/system-information"))
      second (statusCode . responseStatus) res `shouldBe` Right 200
    it "checks that invalid UUIDs always return a 400" $ do
      statuses <-
        runWikiMusic
          ( \cfg ->
              mapM
                (getHttpStatusCode cfg)
                [ "/songs/identifier/abc",
                  "/artists/identifier/abc",
                  "/genres/identifier/abc"
                ]
          )
      second (all (== 400)) statuses `shouldBe` Right True
    it "checks that protected routes are returning 401 when not logged in" $ do
      statuses <-
        runWikiMusic
          ( \cfg ->
              mapM
                (getHttpStatusCode cfg)
                [ "/songs",
                  "/songs/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a",
                  "/artists",
                  "/artists/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a",
                  "/genres",
                  "/genres/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a"
                ]
          )
      second (all (== 401)) statuses `shouldBe` Right True

getHttpStatusCode :: (MonadIO m) => AppConfig -> Text -> m Int
getHttpStatusCode cfg path = do
  res <- liftIO $ httpCall (mkTestUrl cfg <> path)
  pure $ statusCode . responseStatus $ res
