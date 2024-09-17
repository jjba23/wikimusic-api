{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.Test.Integration where

import Control.Concurrent
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Optics
import Relude
import Test.Hspec
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
      let invalidUUIDPaths =
            [ "/songs/identifier/abc",
              "/artists/identifier/abc",
              "/genres/identifier/abc"
            ]
      httpResponses <- runWikiMusic (\cfg -> mapM (\path -> httpCall (mkTestUrl cfg <> path)) invalidUUIDPaths)
      second (all ((== 400) . statusCode . responseStatus)) httpResponses `shouldBe` Right True
    it "checks that protected routes are returning 401 when not logged in" $ do
      let protectedPaths =
            [ "/songs",
              "/songs/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a",
              "/artists",
              "/artists/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a",
              "/genres",
              "/genres/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a"
            ]
      httpResponses <- runWikiMusic (\cfg -> mapM (\path -> httpCall (mkTestUrl cfg <> path)) protectedPaths)
      second (all ((== 401) . statusCode . responseStatus)) httpResponses `shouldBe` Right True
    it "receives empty responses when a user exist but no data exists in DB" $ do
      _ <- runWikiMusic (\cfg -> createUserInDb (cfg ^. #sqlite % #path))
      True `shouldBe` True
