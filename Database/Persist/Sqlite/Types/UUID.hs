{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , QuasiQuotes
  , TemplateHaskell
  , TypeFamilies
  #-}

module Database.Persist.Sqlite.Types.UUID where

import qualified Database.Persist as P
import qualified Database.Persist.Sql as S
import qualified Data.ByteString.Lazy as B
import qualified Data.UUID as U

instance P.PersistField U.UUID where
  toPersistValue u = P.PersistByteString . B.toStrict . U.toByteString $ u

  fromPersistValue (P.PersistByteString t) = case U.fromByteString $ B.fromStrict t of
    Just x  -> Right x
    Nothing -> Left "Invalid UUID"

  fromPersistValue _ = Left "Not PersistBlob"


instance S.PersistFieldSql U.UUID where
   sqlType _ = S.SqlBlob
