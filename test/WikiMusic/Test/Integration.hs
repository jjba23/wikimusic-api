{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

module WikiMusic.Test.Integration where

import WikiMusic.Test.Principium

integrationSpec :: SpecWith ()
integrationSpec =
  describe "starts WikiMusic application properly" $ do
    it "starts and shuts down the application properly" $ do
      result <- testWikiMusic (const . liftIO $ sleepSeconds 1)
      result `shouldBe` ()
    it "checks that the system information route is reachable" $ do
      httpResponse <- testWikiMusic (\cfg -> httpCall Nothing (mkTestUrl cfg <> "/system-information"))
      expectStatus 200 httpResponse
    it "checks that invalid UUIDs always return a 400" $ do
      let invalidUUIDPaths =
            [ "/songs/identifier/abc",
              "/artists/identifier/abc",
              "/genres/identifier/abc"
            ]
      httpResponses <- testWikiMusic (\cfg -> mapM (\path -> httpCall Nothing (mkTestUrl cfg <> path)) invalidUUIDPaths)
      expectAllStatus 400 httpResponses
    it "checks that protected routes are returning 401 when not logged in" $ do
      let protectedPaths =
            [ "/songs",
              "/songs/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a",
              "/artists",
              "/artists/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a",
              "/genres",
              "/genres/identifier/c19e4f56-7c06-437d-b8f7-59d13dd53c9a"
            ]
      httpResponses <- testWikiMusic (\cfg -> mapM (\path -> httpCall Nothing (mkTestUrl cfg <> path)) protectedPaths)
      expectAllStatus 401 httpResponses
    it "receives empty songs list when a user (demo) exists but no data exists in DB" $ do
      httpResponse <-
        testWikiMusic
          ( \cfg -> do
              u <- createUserInDB (cfg ^. #sqlite % #path) "wm::demo"
              httpCall (u ^. #authToken) (mkTestUrl cfg <> "/songs")
          )
      expectStatus 200 httpResponse
      expectResponseBody [trimming|{"songs":{},"sortOrder":[]}|] httpResponse
    it "receives empty genres list when a user (demo) exists but no data exists in DB" $ do
      httpResponse <-
        testWikiMusic
          ( \cfg -> do
              u <- createUserInDB (cfg ^. #sqlite % #path) "wm::demo"
              httpCall (u ^. #authToken) (mkTestUrl cfg <> "/genres")
          )
      expectStatus 200 httpResponse
      expectResponseBody [trimming|{"genres":{},"sortOrder":[]}|] httpResponse
    it "receives empty artists list when a user (demo) exists but no data exists in DB" $ do
      httpResponse <-
        testWikiMusic
          ( \cfg -> do
              u <- createUserInDB (cfg ^. #sqlite % #path) "wm::demo"
              httpCall (u ^. #authToken) (mkTestUrl cfg <> "/artists")
          )
      expectStatus 200 httpResponse
      expectResponseBody [trimming|{"artists":{},"sortOrder":[]}|] httpResponse
