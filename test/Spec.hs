{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.ByteString.Lazy qualified as BL
import Optics
import Relude
import System.Process
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import TestContainers.Tasty qualified as TC

data TestConfig = TestConfig
  { redisHost :: String,
    redisPort :: Int,
    postgreSQLHost :: String,
    postgreSQLPort :: Int
  }
  deriving (Eq, Show, Generic)

makeFieldLabelsNoPrefix ''TestConfig

-- | Sets up and runs the containers required for this test suite.
setupContainers :: (TC.MonadDocker m) => m TestConfig
setupContainers = do
  redisContainer <-
    TC.run
      $ TC.containerRequest (TC.fromTag "redis:6.2-alpine")
      -- Expose the port 6379 from within the container. The respective port
      -- on the host machine can be looked up using `containerPort` (see below).
      TC.& TC.setExpose [6379]
      -- Wait until the container is ready to accept requests. `run` blocks until
      -- readiness can be established.
      TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 6379)
  postgreSQLContainer <-
    TC.run
      $ TC.containerRequest (TC.fromTag "postgres:15.5")
      TC.& TC.setExpose [5432]
      TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 5432)

  pure
    $ TestConfig
      { redisHost = "0.0.0.0",
        redisPort =
          -- Look up the corresponding port on the host machine for the exposed
          -- port 6379.
          TC.containerPort redisContainer 6379,
        postgreSQLHost = "0.0.0.0",
        postgreSQLPort = TC.containerPort postgreSQLContainer 5432
      }

main :: IO ()
main =
  Tasty.defaultMain
    $
    -- Use `withContainers` to make the containers available in the closed over
    -- tests. Due to how Tasty handles resources `withContainers` passes down
    -- an IO action `start` to actually start up the containers. `start` can be
    -- invoked multiple times, Tasty makes sure to only start up the containrs
    -- once.
    --
    -- `withContainers` ensures the started containers are shut down correctly
    -- once execution leaves its scope.
    TC.withContainers setupContainers
    $ \start ->
      Tasty.testGroup
        "Some test group"
        [ Tasty.testCase "Redis + PostgreSQL test" $ do
            -- Actually start the containers!!
            TestConfig {..} <- start
            -- ... assert some properties
            pure (),
          Tasty.testCase "Another Redis + PostgreSQL test" $ do
            -- Invoking `start` twice gives the same Endpoints!
            TestConfig {..} <- start
            (code, stdout, stderr) <- readProcess "ls -l /home" -- >>= BL.putStr
            _ <- BL.putStr stdout
            -- ... assert some properties
            pure (),
          Tasty.testCase "Another Redis + PostgreSQL test 2" $ do
            -- Invoking `start` twice gives the same Endpoints!
            TestConfig {..} <- start
            (code, stdout, stderr) <- readProcess "timeout 20s make dev" -- >>= BL.putStr
            _ <- BL.putStr stdout
            -- ... assert some properties
            pure ()
        ]
