-- {-# OPTIONS_GHC -Wno-orphans #-}

-- module Apps.HazardHunter.DB where

-- import Apps.HazardHunter.Engine (MSLevel)
-- import Data.Text (Text)
-- import Data.Time (UTCTime)
-- import qualified Database.SQLite.Simple as DB
-- import qualified Database.SQLite.Simple.FromField as DB
-- import qualified Database.SQLite.Simple.Internal as DB
-- import qualified Database.SQLite.Simple.Ok as DB
-- import Witch
-- import Prelude

-- data Score = Score
--   { scoreId :: Int,
--     scoreName :: Text,
--     scoreDate :: UTCTime,
--     scoreDuration :: Float,
--     scoreLevel :: MSLevel
--   }
--   deriving (Show)

-- instance DB.FromRow Score where
--   fromRow = Score <$> DB.field <*> DB.field <*> DB.field <*> DB.field <*> DB.field

-- instance DB.FromField MSLevel where
--   fromField (DB.Field (DB.SQLText txt) _) = DB.Ok . from $ txt
--   fromField f = DB.returnError DB.ConversionFailed f "need a valid text level"

-- initDB :: DB.Connection -> IO ()
-- initDB conn =
--   DB.execute_
--     conn
--     "CREATE TABLE IF NOT EXISTS scores (id INTEGER PRIMARY KEY, name TEXT, date DATE, duration REAL, level TEXT)"

-- getTopScores :: DB.Connection -> Integer -> MSLevel -> IO [Score]
-- getTopScores conn limit level =
--   DB.query
--     conn
--     "SELECT * from scores WHERE level = ? ORDER BY duration ASC LIMIT ?"
--     (show level, show limit)

-- addScore :: DB.Connection -> Text -> UTCTime -> Float -> MSLevel -> IO ()
-- addScore conn name date duration level =
--   DB.execute
--     conn
--     "INSERT INTO scores (name, date, duration, level) VALUES (?,?,?,?)"
--     (name, date, duration, show level)
