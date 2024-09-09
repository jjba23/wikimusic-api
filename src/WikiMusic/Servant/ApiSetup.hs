{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Servant.ApiSetup
  ( mkApp,
    PrivateAPI,
    PublicAPI,
    WikiMusicAPIServer,
    wiredUpPrivateServer,
    wiredUpPublicServer,
  )
where

import Data.OpenApi qualified
import Data.Proxy
import Data.Text (unpack)
import Data.Text qualified as T
import Database.Beam
import Database.SQLite.Simple qualified as Sqlite
import Network.Wai
import Network.Wai.Logger (ApacheLogger)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Prometheus qualified as P
import Network.Wai.Middleware.RequestLogger
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
import Relude
import Servant
import Servant.OpenApi
import WikiMusic.Model.Config
import WikiMusic.Model.Env
import WikiMusic.Protolude
import WikiMusic.Servant.ApiSpec
import WikiMusic.Servant.ArtistRoutes
import WikiMusic.Servant.AuthRoutes
import WikiMusic.Servant.GenreRoutes
import WikiMusic.Servant.SongRoutes
import WikiMusic.Servant.UserRoutes
import WikiMusic.Servant.Utilities

swagger :: Servant.Handler Data.OpenApi.OpenApi
swagger = pure $ toOpenApi docsProxy

apiProxy :: Proxy WikiMusicAPIServer
apiProxy = Proxy

docsProxy :: Proxy APIDocsServer
docsProxy = Proxy

myCors :: CorsConfig -> Middleware
myCors cfg = cors (const $ Just policy)
  where
    policy =
      CorsResourcePolicy
        { corsOrigins = Just (map (fromString . unpack) (cfg ^. #origins), True),
          corsMethods = map (fromString . unpack) (cfg ^. #methods),
          corsRequestHeaders = map (fromString . unpack) (cfg ^. #requestHeaders),
          corsExposedHeaders =
            Just
              [ "x-wikimusic-auth",
                "content-type",
                "date",
                "content-length",
                "access-control-allow-origin",
                "access-control-allow-methods",
                "access-control-allow-headers",
                "access-control-request-method",
                "access-control-request-headers"
              ],
          corsMaxAge = Nothing,
          corsVaryOrigin = False,
          corsRequireOrigin = False,
          corsIgnoreFailures = False
        }

mkApp :: (MonadIO m) => ApacheLogger -> AppConfig -> m Application
mkApp logger' cfg = do
  now <- liftIO getCurrentTime
  conn <- liftIO $ Sqlite.open (T.unpack $ cfg ^. #sqlite % #path)
  mailCss <- liftIO $ readFileBS "resources/css/mail.css"

  let env =
        Env
          { cfg = cfg,
            processStartedAt = now,
            logger = logger',
            conn = conn,
            mailCss = T.filter (\x -> x /= '\n' && x /= '\t') . decodeUtf8 $ mailCss
          }
      authCfg = authCheckIO env
      apiCfg = authCfg :. EmptyContext
      apiItself =
        wiredUpPrivateServer env
          :<|> ( swagger
                   :<|> wiredUpPublicServer env
               )

  pure
    . (if (cfg ^. #dev % #reportedVersion) == "dev" then logStdoutDev else logStdout)
    . myCors (cfg ^. #cors)
    . P.prometheus P.def
    $ serveWithContext apiProxy apiCfg apiItself

wiredUpPrivateServer :: Env -> Server PrivateAPI
wiredUpPrivateServer env =
  artistHandlers env :<|> genreHandlers env :<|> songHandlers env :<|> authHandlers env

artistBase :: Env -> Server WithBaseEntityRoutes
artistBase env =
  deleteArtistsByIdentifierRoute env
    :<|> deleteArtistCommentsByIdentifierRoute env
    :<|> deleteArtistOpinionsByIdentifierRoute env
    :<|> deleteArtistArtworksByIdentifierRoute env

artistHandlers :: Env -> Server ArtistsAPI
artistHandlers env =
  artistBase env
    :<|> ( fetchArtistsRoute env
             :<|> searchArtistsRoute env
             :<|> fetchArtistRoute env
             :<|> insertArtistsRoute env
             :<|> insertArtistCommentsRoute env
             :<|> upsertArtistOpinionsRoute env
             :<|> insertArtistArtworksRoute env
             :<|> updateArtistArtworksOrderRoute env
             :<|> updateArtistRoute env
         )

genreBase :: Env -> Server WithBaseEntityRoutes
genreBase env =
  deleteGenresByIdentifierRoute env
    :<|> deleteGenreCommentsByIdentifierRoute env
    :<|> deleteGenreOpinionsByIdentifierRoute env
    :<|> deleteGenreArtworksByIdentifierRoute env

genreHandlers :: Env -> Server GenresAPI
genreHandlers env =
  genreBase env
    :<|> ( fetchGenresRoute env
             :<|> searchGenresRoute env
             :<|> fetchGenreRoute env
             :<|> insertGenresRoute env
             :<|> insertGenreCommentsRoute env
             :<|> upsertGenreOpinionsRoute env
             :<|> insertGenreArtworksRoute env
             :<|> updateGenreArtworksOrderRoute env
             :<|> updateGenreRoute env
         )

songBase :: Env -> Server WithBaseEntityRoutes
songBase env =
  deleteSongsByIdentifierRoute env
    :<|> deleteSongCommentsByIdentifierRoute env
    :<|> deleteSongOpinionsByIdentifierRoute env
    :<|> deleteSongArtworksByIdentifierRoute env

songHandlers :: Env -> Server SongsAPI
songHandlers env =
  songBase env
    :<|> fetchSongsRoute env
    :<|> searchSongsRoute env
    :<|> fetchSongRoute env
    :<|> insertSongsRoute env
    :<|> insertSongCommentsRoute env
    :<|> upsertSongOpinionsRoute env
    :<|> insertSongArtworksRoute env
    :<|> insertArtistOfSongRoute env
    :<|> deleteArtistOfSongRoute env
    :<|> updateSongArtworksOrderRoute env
    :<|> updateSongRoute env
    :<|> insertSongContentsRoute env
    :<|> deleteSongContentsByIdentifierRoute env
    :<|> updateSongContentsRoute env

authHandlers :: Env -> Server AuthAPI
authHandlers env =
  fetchMeRoute env
    :<|> inviteUserRoute env
    :<|> deleteUserRoute env

wiredUpPublicServer :: Env -> Server PublicAPI
wiredUpPublicServer env =
  loginRoute env
    :<|> ( makeResetPasswordLinkRoute env
             :<|> doPasswordResetRoute env
         )
    :<|> systemInformationRoute env
