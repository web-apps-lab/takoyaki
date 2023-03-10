{-# HLINT ignore "Use if" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Apps.HazardHunter.Main where

import Apps.HazardHunter.DB
import Apps.HazardHunter.Engine
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, forever, void, when)
import qualified Data.Map as Map
import Data.Text (Text, intercalate, pack)
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import qualified Database.SQLite.Simple as DB
import qualified Ki
import Lucid
import qualified Network.WebSockets as WS
import Takoyaki.Engine
import Takoyaki.Htmx
import Text.Printf (printf)
import Text.Read (readMaybe)
import Witch
import Prelude

data ServiceEvent
  = StartTimer
  | StopTimer
  deriving (Show)

hazardHunterApp :: IO (App MSState ServiceEvent)
hazardHunterApp = do
  pure $
    App
      { appDesc =
          AppDesc
            "HazardHunter"
            "HazardHunter - Minesweeper like game - Find Hazards and Enter leaderboard !"
            ( "HazardHunter is a mini web game based on the legendary Minesweeper game's logic. "
                <> "Goal is to discover various Hazards by guessing their places on the board. "
                <> "Best scores are stored in the leaderboard !"
            ),
        appMkSessionState,
        appInitDB,
        appRender = renderApp,
        appHandleEvent = handleEvent,
        appService = serviceThread
      }
  where
    appMkSessionState = do
      let level = defaultLevel
      board <- initBoard $ levelToBoardSettings level
      MSState board Wait . MSSettings level "Anonymous" Blue <$> randomHazard
    appInitDB = initDB

diffTimeToFloat :: UTCTime -> UTCTime -> Float
diffTimeToFloat a b = realToFrac $ diffUTCTime a b

serviceThread :: TVar MSState -> TBQueue ServiceEvent -> WS.Connection -> Logger -> IO ()
serviceThread stateV serviceQ conn _logger = do
  timerState <- newTVarIO Nothing
  appState' <- readTVarIO stateV
  when (isPlaying appState'.state) $ atomically $ writeTBQueue serviceQ StartTimer
  Ki.scoped $ \scope -> do
    Ki.fork_ scope $ forever $ do
      event <- atomically $ readTBQueue serviceQ
      case event of
        StartTimer -> do
          atomically $ do
            appState <- readTVar stateV
            writeTVar timerState $ getPlayDate appState.state
          void $ Ki.fork scope $ timerT timerState
        StopTimer -> atomically $ writeTVar timerState Nothing
    atomically $ Ki.awaitAll scope
  where
    timerT :: TVar (Maybe UTCTime) -> IO ()
    timerT timerState = go
      where
        go = do
          ts <- readTVarIO timerState
          case ts of
            Just atTime -> do
              now <- getCurrentTime
              WS.sendTextData conn
                . renderBS
                . renderTimer
                $ diffTimeToFloat now atTime
              threadDelay 500000
              go
            Nothing -> pure ()
    getPlayDate :: MSGameState -> Maybe UTCTime
    getPlayDate (Play date _) = Just date
    getPlayDate _ = Nothing
    isPlaying :: MSGameState -> Bool
    isPlaying (Play _ _) = True
    isPlaying _ = False

handleEvent :: WSEvent -> TVar MSState -> TBQueue ServiceEvent -> DB.Connection -> Logger -> IO [Html ()]
handleEvent wEv appStateV serviceQ dbConn logger = do
  case wSEvent wEv of
    Just (ClickCell cellCoord) -> do
      atTime <- getCurrentTime
      appState' <- readTVarIO appStateV
      case countOpenCells appState'.board of
        0 -> do
          -- Ensure the first click on the board is not a hazard
          newBoard <- ensureNFBoard appState'.board cellCoord appState'.settings.level
          atomically $ modifyTVar' appStateV $ \s -> s {board = newBoard}
          pure ()
        _ -> pure ()
      case appState'.state of
        Wait -> atomically $ do
          modifyTVar' appStateV $ \s -> s {state = Play atTime False}
          writeTBQueue serviceQ StartTimer
        _ -> pure ()
      appState <- readTVarIO appStateV
      case appState.state of
        Play _ False -> do
          let playDuration = mkPlayDuration appState.state atTime
          case isFlagCell cellCoord appState.board of
            True -> pure []
            False -> do
              case isMineCell cellCoord appState.board of
                True -> do
                  (board, panel) <- atomically $ do
                    writeTBQueue serviceQ StopTimer
                    modifyTVar' appStateV $ \s ->
                      s
                        { board = openCell cellCoord appState.board,
                          state = Gameover
                        }
                    board <- renderBoard appStateV
                    panel <- renderPanel appStateV (Just playDuration)
                    pure (board, panel)
                  pure [board, panel]
                False -> do
                  let gs1 = openCell cellCoord appState.board
                      gs2 = openAdjBlank0Cells (levelToBoardSettings appState.settings.level) cellCoord gs1
                  case countHiddenBlank gs2 == 0 of
                    True -> do
                      (board, panel) <- atomically $ do
                        writeTBQueue serviceQ StopTimer
                        modifyTVar' appStateV $ \s -> s {board = gs2, state = Win}
                        board <- renderBoard appStateV
                        panel <- renderPanel appStateV (Just playDuration)
                        pure (board, panel)
                      addScore dbConn appState.settings.playerName atTime playDuration appState.settings.level
                      leaderBoard <- renderLeaderBoard appStateV dbConn
                      pure [board, panel, leaderBoard]
                    False -> do
                      (board, smiley) <- atomically $ do
                        modifyTVar' appStateV $ \s -> s {board = gs2}
                        board <- renderBoard appStateV
                        smiley <- renderSmiley appStateV
                        pure (board, smiley)
                      pure [board, smiley]
        Play _ True -> atomically $ do
          let board = setFlagOnCell cellCoord appState.board
          modifyTVar' appStateV $ \s -> s {board}
          flag <- renderFlag appStateV
          board' <- renderBoard appStateV
          pure [flag, board']
        _ -> pure []
    Just NewGame -> atomically $ do
      writeTBQueue serviceQ StopTimer
      panel <- renderPanel appStateV (Just 0.0)
      levelsSelector <- renderSettings appStateV
      pure [panel, levelsSelector]
    Just (SettingsSelected level playerName boardColor) -> do
      newBoard <- initBoard $ levelToBoardSettings level
      hazard <- randomHazard
      atomically $ do
        modifyTVar' appStateV $ \s ->
          s
            { board = newBoard,
              state = Wait,
              settings = MSSettings level playerName boardColor hazard
            }
      app <- renderApp appStateV logger dbConn
      pure [app]
    Just SetFlagMode -> do
      frags <- atomically $ do
        appState <- readTVar appStateV
        case appState.state of
          Play st fm -> do
            modifyTVar' appStateV $ \s -> s {state = Play st (not fm)}
            renderFlag appStateV
          _ -> pure $ pure ()
      pure [frags]
    Nothing -> pure []
  where
    mkPlayDuration :: MSGameState -> UTCTime -> Float
    mkPlayDuration s curD = case s of
      Play startDate _ -> diffTimeToFloat curD startDate
      _ -> error "Should not happen"
    wSEvent :: WSEvent -> Maybe MSEvent
    wSEvent (WSEvent wseName _ wseData) = do
      let getData = flip Map.lookup wseData
      case wseName of
        "clickCell" -> do
          case (getData "cx", getData "cy") of
            (Just (Just cxS), Just (Just cyS)) -> do
              let cxM = readMaybe $ from cxS :: Maybe Int
                  cyM = readMaybe $ from cyS :: Maybe Int
              case (cxM, cyM) of
                (Just cx, Just cy) -> Just $ ClickCell (MSCellCoord cx cy)
                _ -> Nothing
            _ -> Nothing
        "play" -> Just NewGame
        "setSettings" -> do
          case (getData "level", getData "playerName", getData "boardColor") of
            (Just (Just level), Just (Just playerName), Just (Just boardColor)) ->
              Just $
                SettingsSelected (from level) playerName (from boardColor)
            _ -> Nothing
        "setFlagMode" -> Just SetFlagMode
        _ -> Nothing
    ensureNFBoard :: MSBoard -> MSCellCoord -> MSLevel -> IO MSBoard
    ensureNFBoard board cellCoord level = case isMineCell cellCoord board of
      True -> do
        newBoard <- initBoard $ levelToBoardSettings level
        ensureNFBoard newBoard cellCoord level
      False -> pure board

withThemeBgColor :: Color -> Text -> Text -> Text
withThemeBgColor = withThemeColor "bg"

withThemeBorderColor :: Color -> Text -> Text -> Text
withThemeBorderColor = withThemeColor "border"

withThemeColor :: Text -> Color -> Text -> Text -> Text
withThemeColor cat color level cur = cur <> " " <> bgColorBase <> "-" <> level
  where
    colorBase = case color of
      Blue -> "blue"
      Pink -> "pink"
      Green -> "green"
    bgColorBase = cat <> "-" <> colorBase

renderApp :: TVar MSState -> Logger -> DB.Connection -> IO (Html ())
renderApp appStateV _logger dbConn = do
  (panel, board) <- atomically $ do
    panel <- renderPanel appStateV (Just 0.0)
    board <- renderBoard appStateV
    pure (panel, board)
  leaderBoard <- renderLeaderBoard appStateV dbConn
  appState <- readTVarIO appStateV
  pure $ div_ [id_ "MSMain", class_ "min-w-fit max-w-fit border-2 rounded border-gray-400 bg-gray-100"] $ do
    div_ [class_ "flex flex-col"] $ do
      div_ [class_ "border-solid rounded border-2 m-1 border-gray-300"] $ do
        panel
        board
      div_ [class_ "border-solid rounded border-2 m-1 border-gray-300"] $ do
        renderLeaderBoardHeader appState.settings.level appState.settings.color
        leaderBoard
      div_ [class_ $ withThemeBgColor appState.settings.color "200" ""] $ do
        div_ [class_ "flex flex-row gap-2 flex-row-reverse pr-2"] $ do
          div_ [] "- 1.0.0"
          a_ [class_ "text-blue-600"] "HazardHunter"

renderLeaderBoardHeader :: MSLevel -> Color -> Html ()
renderLeaderBoardHeader level color =
  div_ [class_ $ withThemeBgColor color "200" "text-center"] $ toHtml (show level) <> " " <> "Leaderboard"

renderLeaderBoard :: TVar MSState -> DB.Connection -> IO (Html ())
renderLeaderBoard appStateV dbConn = do
  appState <- readTVarIO appStateV
  scores <- getTopScores dbConn 10 appState.settings.level
  pure $
    div_ [id_ "MSLeaderBoard", class_ $ withThemeBgColor appState.settings.color "100" ""] $
      case length scores of
        0 -> p_ "The leaderboard is empty. Be the first to appear here !"
        _ -> ol_ [] $ mapM_ displayScoreLine scores
  where
    displayScoreLine :: Score -> Html ()
    displayScoreLine Score {..} = do
      li_ [] $ div_ [class_ "grid grid-cols-5 gap-1"] $ do
        div_ [class_ "col-span-4"] $
          toHtml $
            formatTime defaultTimeLocale "%F" scoreDate <> " " <> from scoreName
        div_ [class_ "col-span-1 text-right"] $ toHtml (toDurationT scoreDuration)

renderPanel :: TVar MSState -> Maybe Float -> STM (Html ())
renderPanel appStateV durationM = do
  smiley <- renderSmiley appStateV
  flag <- renderFlag appStateV
  appState <- readTVar appStateV
  pure $ div_ [id_ "MSPanel", class_ $ withThemeBgColor appState.settings.color "200" "flex justify-between"] $ do
    let mineCount' = mineCount $ levelToBoardSettings appState.settings.level
    div_ [class_ "pl-1 w-24"] $ toHtml $ hazardLabel mineCount' appState.settings.hazard
    div_ [class_ "flex flex-row gap-2"] flag
    forM_ durationM renderTimer
    smiley
  where
    hazardLabel :: Int -> Hazard -> Text
    hazardLabel count hazard = from (show count) <> " " <> hazardToText hazard

renderFlag :: TVar MSState -> STM (Html ())
renderFlag appStateV = do
  appState <- readTVar appStateV
  let flagMode = case appState.state of
        Play _ True ->
          withThemeBgColor appState.settings.color "300" ""
        _ -> mempty
      usedFlags = countFlagCells appState.board
  pure $ div_ [id_ "MSFlag"] $ do
    div_ [class_ "flex flex-row gap-1"] $ do
      withEvent "setFlagMode" [] $
        div_ [class_ $ withThemeBorderColor appState.settings.color "300" "cursor-pointer border-2 rounded" <> flagMode] "????"
      div_ . toHtml $ "(" <> show usedFlags <> ")"

renderSmiley :: TVar MSState -> STM (Html ())
renderSmiley appStateV = do
  appState <- readTVar appStateV
  pure $
    div_ [id_ "MSSmiley"] $
      withEvent "play" [] $
        div_ [class_ $ withThemeBorderColor appState.settings.color "300" "px-1 cursor-pointer border-2 rounded whitespace-nowrap"] $
          ( case appState.state of
              Play _ _ -> "????"
              Wait -> "????"
              Gameover -> "????"
              Win -> "????"
          )
            <> " New Game"

toDurationT :: Float -> String
toDurationT = printf "%.1f"

renderTimer :: Float -> Html ()
renderTimer duration = do
  div_ [id_ "MSTimer", class_ "w-24 text-right pr-1"] $ toHtml $ toDurationT duration

renderSettings :: TVar MSState -> STM (Html ())
renderSettings appStateV = do
  appState <- readTVar appStateV
  let playerName = appState.settings.playerName
      selectedLevel = appState.settings.level
  pure $ div_ [id_ "MSBoard"] $ do
    withEvent "setSettings" [] $ do
      form_ [class_ $ withThemeBgColor appState.settings.color "100" "flex flex-col items-center gap-px"] $ do
        div_ [class_ "my-2"] $ startButton appState
        label_ [class_ "m-1 font-semibold"] "Set the board color"
        colorInput appState.settings.color
        label_ [class_ "m-1 font-semibold"] "Set your name"
        nameInput playerName
        label_ [class_ "m-1 font-semibold"] "Select a level"
        mapM_ (div_ . levelButton selectedLevel) [minBound .. maxBound]
  where
    colorInput :: Color -> Html ()
    colorInput colorFromSettings =
      select_
        [ class_ "block mb-2 text-center border border-slate-300 rounded-md",
          name_ "boardColor"
        ]
        $ mapM_ (\c -> option_ (setValue c) (toHtml $ show c)) [minBound .. maxBound]
      where
        setValue :: Color -> [Attribute]
        setValue color =
          let selected = if colorFromSettings == color then [selected_ ""] else mempty
           in [value_ . from $ show color] <> selected
    nameInput :: Text -> Html ()
    nameInput playerName =
      input_
        [ type_ "text",
          name_ "playerName",
          value_ playerName,
          placeholder_ "Anonymous",
          size_ "15",
          maxlength_ "15",
          class_ "h-8 text-center border border-slate-300 rounded-md focus:border-slate-400"
        ]
    startButton :: MSState -> Html ()
    startButton appState =
      button_
        [ type_ "submit",
          class_ $ withThemeBorderColor appState.settings.color "300" "p-1 border-4 rounded"
        ]
        $ div_ [class_ "px-6 font-bold"] "Play"
    levelButton :: MSLevel -> MSLevel -> Html ()
    levelButton selectedLevel level = do
      input_ $
        [name_ "level", id_ levelT, type_ "radio", value_ levelT]
          <> if level == selectedLevel then [checked_] else mempty
      label_ [for_ levelT] $ span_ [class_ levelS] $ toHtml levelT
      where
        levelT :: Text
        levelT = from $ show level
        levelS =
          "ml-2" <> " " <> case level of
            Baby -> "text-blue-700"
            Beginner -> "text-blue-800"
            Intermediate -> "text-green-700"
            Expert -> "text-red-700"
            Specialist -> "text-red-900"
            Survivalist -> "text-violet-900"

renderBoard :: TVar MSState -> STM (Html ())
renderBoard appStateV = do
  appState <- readTVar appStateV
  let sizeCount' = sizeCount $ levelToBoardSettings appState.settings.level
  let gridType = "grid-cols-[" <> intercalate "_" (Prelude.replicate (sizeCount' + 1) "20px") <> "]"
  pure $ div_ [id_ "MSBoard"] $ do
    div_ [class_ "flex place-content-center m-1"] $ do
      div_ [class_ $ "grid gap-1 " <> gridType] $ do
        mapM_ (renderCell appState.state appState.settings.hazard appState.settings.color) $
          Map.toList appState.board
  where
    renderCell :: MSGameState -> Hazard -> Color -> (MSCellCoord, MSCell) -> Html ()
    renderCell gameState hazard color (cellCoords, cellState) =
      let cellId = mkHxVals [("cx", pack $ show $ cellCoords.cx), ("cy", pack $ show $ cellCoords.cy)]
       in installCellEvent gameState cellId $
            div_ [class_ $ withThemeBgColor color "100" "text-center cursor-pointer"] $
              case cellState of
                MSCell (Blank v) Open
                  | v == 0 -> div_ [class_ "h-6 w-full "] ""
                  | v == 1 -> div_ [class_ "font-bold text-blue-700"] $ showCellValue v
                  | v == 2 -> div_ [class_ "font-bold text-green-700"] $ showCellValue v
                  | v == 3 -> div_ [class_ "font-bold text-red-700"] $ showCellValue v
                  | v == 4 -> div_ [class_ "font-bold text-blue-900"] $ showCellValue v
                  | v == 5 -> div_ [class_ "font-bold text-red-900"] $ showCellValue v
                  | v == 6 -> div_ [class_ "font-bold text-green-900"] $ showCellValue v
                  | v == 7 -> div_ [class_ "font-bold text-brown-700"] $ showCellValue v
                  | v == 8 -> div_ [class_ "font-bold text-black-700"] $ showCellValue v
                MSCell (Blank _) Open -> error "Impossible case"
                MSCell Mine Open -> mineCell
                MSCell (Blank _) (Hidden True) -> flagCell
                MSCell Mine (Hidden flag) -> case gameState of
                  Gameover -> mineCell
                  _ -> if flag then flagCell else hiddenCell
                MSCell _ _ -> hiddenCell
      where
        mineCell = div_ [class_ "bg-red-500"] $ toHtml $ hazardToText hazard
        hiddenCell = div_ [class_ $ withThemeBgColor color "300" "border-2 rounded border-r-gray-400 border-b-gray-400 h-6 w-full"] ""
        flagCell = div_ [class_ $ withThemeBgColor color "300" "border-2 rounded border-r-gray-400 border-b-gray-400 h-6 w-full"] "????"
        showCellValue :: Int -> Html ()
        showCellValue = toHtml . show
        installCellEvent :: MSGameState -> Attribute -> Html () -> Html ()
        installCellEvent gs cellId elm =
          let elm' = withEvent "clickCell" [cellId] elm
           in case gs of
                Play _ _ -> elm'
                Wait -> elm'
                _ -> elm

run :: Int -> IO ()
run port = do
  app <- hazardHunterApp
  runServer port app
