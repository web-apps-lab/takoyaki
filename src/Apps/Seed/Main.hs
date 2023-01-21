module Apps.Seed.Main where

import Apps.Seed.DB
import Apps.Seed.Engine
import Control.Concurrent.STM
import qualified Database.SQLite.Simple as DB
import Lucid
import qualified Network.WebSockets as WS
import Takoyaki.Engine
import Prelude

data ServiceEvent

seedApp :: IO (App AppState ServiceEvent)
seedApp = do
  pure $
    App
      { appDesc = AppDesc "Seed" "The Seed app" "",
        appMkSessionState = pure $ AppState "Hello App",
        appInitDB = initDB,
        appRender = renderApp,
        appHandleEvent = handleEvent,
        appService = serviceThread
      }

serviceThread :: TVar AppState -> TBQueue ServiceEvent -> WS.Connection -> Logger -> IO ()
serviceThread _stateV _serviceQ _conn _logger = pure ()

handleEvent :: WSEvent -> TVar AppState -> TBQueue ServiceEvent -> DB.Connection -> Logger -> IO [Html ()]
handleEvent _wEv _appStateV _serviceQ _dbConn _logger = pure [pure ()]

renderApp :: TVar AppState -> Logger -> DB.Connection -> IO (Html ())
renderApp appStateV _logger _dbConn = do
  appState <- readTVarIO appStateV
  pure $
    div_ [id_ "MSMain", class_ ""] $ do
      p_ $ toHtml appState.seed

run :: Int -> IO ()
run port = do
  app <- seedApp
  runServer port app
