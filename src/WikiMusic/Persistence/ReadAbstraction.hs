{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Persistence.ReadAbstraction
  ( persistenceReadCall,
    fetchOpinions,
    fetchArtworks,
    fetchComments,
    mkSearchConstraints,
    bindParams,
  )
where

import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Text hiding (map)
import Data.Time
import Data.UUID hiding (fromString)
import Data.Vector qualified as V
import Hasql.Decoders as D
import Hasql.Encoders as E
import Hasql.Pool qualified
import Hasql.Session qualified
import Hasql.Statement (Statement (..))
import NeatInterpolation
import Relude

bindParams :: Int -> Text
bindParams size = Data.Text.intercalate ", " (["$" <> show x | x <- [1 .. size]])

persistenceReadCall ::
  (MonadIO m) =>
  Hasql.Pool.Pool ->
  Hasql.Session.Session b1 ->
  ([a] -> b2) ->
  (b1 -> [a]) ->
  m b2
persistenceReadCall pool sessionCreateF responseCreateF vectorParseF = do
  eitherItems <- liftIO $ Hasql.Pool.use pool sessionCreateF
  either
    ( \err -> do
        liftIO . BL.putStr . fromString . show $ err
        pure $ responseCreateF []
    )
    (pure . responseCreateF . vectorParseF)
    eitherItems

fetchOpinions ::
  (MonadIO m, Ord k, Foldable foldable) =>
  Hasql.Pool.Pool ->
  ( [ ( UUID,
        UUID,
        UUID,
        Bool,
        Bool,
        UTCTime,
        Maybe UTCTime
      )
    ] ->
    [(k, a)]
  ) ->
  Text ->
  Text ->
  foldable UUID ->
  m (Map k a)
fetchOpinions pool parseRow entityTable entityIdentifier identifiers = do
  persistenceReadCall
    pool
    (Hasql.Session.statement identifiers preparedStmt)
    Map.fromList
    (parseRow . V.toList)
  where
    preparedStmt = Statement query encoder decoder True
    query =
      encodeUtf8
        [untrimming|
      SELECT identifier, $entityIdentifier, created_by, is_like, is_dislike,
      created_at, last_edited_at FROM $entityTable WHERE $entityIdentifier = ANY($$1)
    |]
    encoder =
      E.param . E.nonNullable $ (E.foldableArray . E.nonNullable $ E.uuid)

    decoder = D.rowVector vector
      where
        vector =
          (,,,,,,)
            <$> D.column (D.nonNullable D.uuid)
            <*> D.column (D.nonNullable D.uuid)
            <*> D.column (D.nonNullable D.uuid)
            <*> D.column (D.nonNullable D.bool)
            <*> D.column (D.nonNullable D.bool)
            <*> D.column (D.nonNullable D.timestamptz)
            <*> D.column (D.nullable D.timestamptz)

fetchArtworks ::
  (MonadIO m, Ord k, Foldable foldable) =>
  Hasql.Pool.Pool ->
  ( [ ( UUID,
        UUID,
        UUID,
        Int64,
        Maybe UUID,
        Text,
        Maybe Text,
        UTCTime,
        Maybe UTCTime,
        Int64
      )
    ] ->
    [(k, a)]
  ) ->
  Text ->
  Text ->
  foldable UUID ->
  m (Map k a)
fetchArtworks pool parseRow entityTable entityIdentifier identifiers = do
  persistenceReadCall
    pool
    (Hasql.Session.statement identifiers preparedStmt)
    Map.fromList
    (parseRow . V.toList)
  where
    preparedStmt = Statement query encoder decoder True
    query =
      encodeUtf8
        [untrimming|
    SELECT identifier, $entityIdentifier, created_by, visibility_status, approved_by, content_url, content_caption,
    created_at, last_edited_at, order_value FROM $entityTable WHERE $entityIdentifier = ANY($$1) ORDER BY ${entityTable}.order_value ASC
    |]
    encoder = E.param . E.nonNullable $ (E.foldableArray . E.nonNullable $ E.uuid)
    decoder = D.rowVector vector
      where
        vector =
          (,,,,,,,,,)
            <$> D.column (D.nonNullable D.uuid)
            <*> D.column (D.nonNullable D.uuid)
            <*> D.column (D.nonNullable D.uuid)
            <*> D.column (D.nonNullable D.int8)
            <*> D.column (D.nullable D.uuid)
            <*> D.column (D.nonNullable D.text)
            <*> D.column (D.nullable D.text)
            <*> D.column (D.nonNullable D.timestamptz)
            <*> D.column (D.nullable D.timestamptz)
            <*> D.column (D.nonNullable D.int8)

fetchComments ::
  (MonadIO m, Ord k, Foldable foldable) =>
  Hasql.Pool.Pool ->
  ( [ ( UUID,
        UUID,
        Maybe UUID,
        UUID,
        Int64,
        Text,
        Maybe UUID,
        UTCTime,
        Maybe UTCTime
      )
    ] ->
    [(k, a)]
  ) ->
  Text ->
  Text ->
  foldable UUID ->
  m (Map k a)
fetchComments pool parseRow entityTable entityIdentifier identifiers = do
  persistenceReadCall
    pool
    (Hasql.Session.statement identifiers preparedStmt)
    Map.fromList
    (parseRow . V.toList)
  where
    preparedStmt = Statement query encoder decoder True
    query =
      encodeUtf8
        [untrimming|
      SELECT identifier, $entityIdentifier, parent_identifier, created_by, visibility_status,
      contents, approved_by, created_at, last_edited_at FROM $entityTable
      WHERE $entityIdentifier = ANY($$1) ORDER BY ${entityTable}.created_at ASC
    |]
    encoder =
      E.param . E.nonNullable $ (E.foldableArray . E.nonNullable $ E.uuid)
    decoder = D.rowVector vector
      where
        vector =
          (,,,,,,,,)
            <$> D.column (D.nonNullable D.uuid)
            <*> D.column (D.nonNullable D.uuid)
            <*> D.column (D.nullable D.uuid)
            <*> D.column (D.nonNullable D.uuid)
            <*> D.column (D.nonNullable D.int8)
            <*> D.column (D.nonNullable D.text)
            <*> D.column (D.nullable D.uuid)
            <*> D.column (D.nonNullable D.timestamptz)
            <*> D.column (D.nullable D.timestamptz)

mkSearchConstraints :: Text -> Text
mkSearchConstraints searchInput = Data.Text.intercalate " AND " inputs
  where
    cleanWords = map (Data.Text.filter (\x -> x /= '\'' && x /= '%' && x /= ';' && x /= '"') . strip) (Data.Text.words searchInput)
    inputs = map (\x -> [untrimming|artists.display_name ILIKE '%$x%'|]) cleanWords
