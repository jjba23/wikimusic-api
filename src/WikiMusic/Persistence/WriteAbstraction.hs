{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.Persistence.WriteAbstraction
  ( deleteStuffByUUID,
    hasqlTransaction,
    incrementViewsByOne',
  )
where

import Control.Concurrent
import Data.Text
import Data.UUID hiding (fromString)
import Data.Vector qualified as V
import Hasql.Decoders as D
import Hasql.Encoders as E
import Hasql.Pool qualified
import Hasql.Statement (Statement (..))
import Hasql.Transaction
import Hasql.Transaction.Sessions
import NeatInterpolation
import Relude
import WikiMusic.Protolude

deleteStuffByUUID :: (MonadIO m) => Hasql.Pool.Pool -> Text -> Text -> [UUID] -> m (Either Hasql.Pool.UsageError ())
deleteStuffByUUID pool entityTable entityIdentifier identifiers = do
  hasqlTransaction pool stmt (V.fromList identifiers)
  where
    stmt = Statement query encoder decoder True
    query =
      encodeUtf8
        [trimming|
        DELETE FROM $entityTable WHERE ${entityTable}.${entityIdentifier} = ANY($$1)                     
        |]
    encoder = E.param . E.nonNullable $ (E.foldableArray . E.nonNullable $ E.uuid)
    decoder = D.noResult

hasqlTransaction ::
  (MonadIO m) =>
  Hasql.Pool.Pool ->
  Statement row model ->
  row ->
  m (Either Hasql.Pool.UsageError model)
hasqlTransaction pool stmt row = liftIO $ Hasql.Pool.use pool transaction'
  where
    stmt' = Hasql.Transaction.statement row stmt
    transaction' = transaction Serializable Write stmt'

incrementViewsByOne' :: (MonadIO m) => Env -> [UUID] -> Text -> m (Either a ())
incrementViewsByOne' env identifiers entityTable = do
  Right <$> (void . liftIO . forkIO $ mapM_ performUpdate identifiers)
  where
    stmt = Statement query encoder D.noResult True
    encoder = E.param . E.nonNullable $ E.uuid
    performUpdate x = do
      operationResults <- hasqlTransaction (env ^. #pool) stmt x
      pure $ first (pack . show) operationResults
    query =
      encodeUtf8
        [trimming|
        UPDATE $entityTable SET views = views + 1 WHERE identifier = $$1
        |]
