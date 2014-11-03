{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , OverloadedStrings
  , QuasiQuotes
  , TemplateHaskell
  , TypeFamilies
  #-}

import Data.Maybe
import Data.UUID (UUID)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
       share, sqlSettings)
import qualified Data.UUID as UUID
import Database.Persist.Types.UUID
import Control.Monad.Error

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Foo
  bar UUID
  FooU bar
  deriving Show
|]

main :: IO ()
main = runSqlite ":memory": $ do 
    runMigration migrateAll
    let u = fromJust $ UUID.fromString "497B3086-0AE2-4433-BF55-EDF3F5F78399"
    insert $ Foo u
    res <- getBy $ FooU u
    liftIO $ print res
	dumpTable
