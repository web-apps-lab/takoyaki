module Apps.Seed.DB where

import qualified Database.SQLite.Simple as DB
import Prelude

initDB :: DB.Connection -> IO ()
initDB _conn = pure ()
