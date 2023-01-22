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
            div_ [class_ "flex flex-row flex-wrap"] $ do
              div_ [class_ "basis-1/2 min-w-fit grow bg-green-400"] $ do
                div_ [class_ "m-2 grid grid-flow-row-dense gap-2 grid-cols-6 grid-rows-3"] $ do
                  mapM_ (\_ -> div_ [class_ "w-14 h-14 shadow shadow-black bg-yellow-100 border-2 rounded border-pink-100"] "1") [1 .. 24]
              div_ [class_ "basis-1/2 min-w-min grow bg-pink-400"] $ do
                p_ "LeaderBoard"
                p_ "Elsa"
                p_ "Fabien"
          div_ [class_ "bg-indigo-100"] $ do
            p_ $ toHtml appState.seed

run :: Int -> IO ()
run port = do
  app <- seedApp
  runServer port app
