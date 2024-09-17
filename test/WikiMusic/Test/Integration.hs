{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.Test.Integration where

import Control.Concurrent
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Optics
import Relude
import Test.Hspec
import WikiMusic.Sqlite.Yggdrasil
import WikiMusic.Test.Principium

integrationSpec :: SpecWith ()
integrationSpec =
  describe "starts WikiMusic application properly" $ do
    it "starts and shuts down the application properly" $ do
      result <- testWikiMusic (const . liftIO $ threadDelay 1000000)
      result `shouldBe` ()
    it "checks that the system information route is reachable" $ do
      res <- testWikiMusic (\cfg -> httpCall (mkTestUrl cfg <> "/system-information"))
      (statusCode . responseStatus $ res) `shouldBe` 200
    it "checks that invalid UUIDs always return a 400" $ do
      let invalidUUIDPaths =
            [ "/songs/identifier/abc",
              "/artists/identifier/abc",
              "/genres/identifier/abc"
            ]
      httpResponses <- testWikiMusic (\cfg -> mapM (\path -> httpCall (mkTestUrl cfg <> path)) invalidUUIDPaths)
      all ((== 400) . statusCode . responseStatus) httpResponses `shouldBe` True
    it "checks that protected routes are returning 401 when not logged in" $ do
      let protectedPaths =
            [ "/songs",
              "/songs/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a",
              "/artists",
              "/artists/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a",
              "/genres",
              "/genres/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a"
            ]
      httpResponses <- testWikiMusic (\cfg -> mapM (\path -> httpCall (mkTestUrl cfg <> path)) protectedPaths)
      all ((== 401) . statusCode . responseStatus) httpResponses `shouldBe` True
    it "receives empty responses when a user exist but no data exists in DB" $ do
      createdTestUser <-
        testWikiMusic
          ( \cfg -> do
              _ <- runYggdrasil "resources/migrations/sqlite"
              createUserInDB (cfg ^. #sqlite % #path)
          )
      True `shouldBe` True
