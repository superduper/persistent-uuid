{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , QuasiQuotes
  , TemplateHaskell
  , TypeFamilies
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

import Data.Maybe
import qualified Data.UUID as U

import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
       share, sqlSettings)

import qualified Database.Persist.Sqlite.Types.UUID()

import Control.Monad.Error

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Foo
  bar U.UUID
  FooU bar
  deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do 
    runMigration migrateAll
    let u = fromJust $ U.fromString "497B3086-0AE2-4433-BF55-EDF3F5F78399"
    insert $ Foo u
    res <- getBy $ FooU u
    liftIO $ print res
