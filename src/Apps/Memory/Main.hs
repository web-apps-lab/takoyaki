module Apps.Memory.Main where

import Apps.Memory.DB
import Apps.Memory.Engine
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
      { appDesc = AppDesc "Memery" "The Memory app" "",
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
    div_ [id_ "AppMain", class_ "flex flex-row justify-center"] $ do
      div_ [class_ "grow min-w-min max-w-screen-2xl bg-blue-400"] $ do
        div_ [class_ "flex-col justify-between h-full"] $ do
          div_ [class_ "bg-indigo-100"] $ do
            p_ $ toHtml appState.seed
          div_ [class_ "bg-red-100"] $ do
            mapM_ (\_w -> p_ $ toHtml appState.seed) ([1 .. 10] :: [Int])
          div_ [class_ "bg-indigo-100"] $ do
            p_ $ toHtml appState.seed

run :: Int -> IO ()
run port = do
  app <- seedApp
  runServer port app
