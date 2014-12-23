{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , QuasiQuotes
  , TemplateHaskell
  , TypeFamilies
  #-}

module Database.Persist.Postgresql.Types.UUID where

import qualified Database.Persist as P
import Database.Persist.Sql
import qualified Data.ByteString.Char8 as B
import qualified Data.UUID as UUID

instance PersistField UUID.UUID where
  toPersistValue u = PersistDbSpecific . B.pack . ("" ++) . (++ "") . UUID.toString $ u

  fromPersistValue (PersistDbSpecific t) = case UUID.fromString $ B.unpack t of
    Just x  -> Right x
    Nothing -> Left "Invalid UUID"

  fromPersistValue _ = Left "Not PersistDBSpecific"

instance PersistFieldSql UUID.UUID where
   sqlType _ = SqlOther "UUID"
