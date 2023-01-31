{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use zipWith" #-}
module Apps.Memory.Main where

import Apps.Memory.DB
import Apps.Memory.Engine
import Control.Concurrent.STM
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Char8 (unpack)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as DB
import Debug.Trace
import Lucid
import qualified Network.WebSockets as WS
import Takoyaki.Engine
import Takoyaki.Htmx
import Text.Read (readMaybe)
import Witch
import Prelude

data ServiceEvent

memoryApp :: IO (App AppState ServiceEvent)
memoryApp = do
  svgs <- loadSVG
  pure $
    App
      { appDesc = AppDesc "Memory" "The Memory app" "",
        appMkSessionState = do
          (board, collectionName) <- mkBoard svgs
          pure $
            AppState
              { collectionName,
                board,
                gameState = Play $ Progress NoCardTurned,
                cardsToRender = mempty
              },
        appInitDB = initDB,
        appRender = renderApp svgs,
        appHandleEvent = handleEvent svgs,
        appService = serviceThread
      }

serviceThread :: TVar AppState -> TBQueue ServiceEvent -> WS.Connection -> Logger -> IO ()
serviceThread _stateV _serviceQ _conn _logger = pure ()

handleEvent :: SVGCollections -> WSEvent -> TVar AppState -> TBQueue ServiceEvent -> DB.Connection -> Logger -> IO [Html ()]
handleEvent cols (WSEvent wseName _ wseData) appStateV _serviceQ _dbConn logger = do
  let getData = flip Map.lookup wseData
  case wseName of
    "clickCard" -> do
      case getData "index" of
        Just (Just index) -> do
          logger $ "Click on " <> show index
          appState <- atomically $ do
            modifyTVar appStateV flipSuccFail
            modifyTVar appStateV (handleCardClick $ unsafeToInt index)
            readTVar appStateV
          mapM (renderCard cols appStateV) appState.cardsToRender
        _ -> pure mempty
    "clickRestart" -> do
      (board, _) <- mkBoard cols
      atomically $ do
        modifyTVar appStateV $ \appState ->
          appState
            { board,
              cardsToRender = mempty,
              gameState = Play $ Progress NoCardTurned
            }
      boardHtml <- renderBoard cols appStateV
      pure [boardHtml]
    "clickMenu" -> do
      pure []
    _ -> pure mempty
  where
    handleCardClick :: CardId -> AppState -> AppState
    handleCardClick cardId appState =
      let newAppState = case appState.gameState of
            Play Wait -> appState {gameState = Play (Progress NoCardTurned)}
            Play (Progress NoCardTurned) ->
              appState
                { gameState = Play (Progress $ OneCardTurned cardId),
                  board = setCardStatus cardId TurnedWaitPair appState.board,
                  cardsToRender = appState.cardsToRender <> [cardId]
                }
            Play (Progress (OneCardTurned turnedCardId)) -> do
              let isPairTurned =
                    getCardName appState.board turnedCardId == getCardName appState.board cardId
                      && turnedCardId /= cardId
                  (board, cardsToRender)
                    | isPairTurned =
                        ( setCardStatus cardId TurnedMatchSucc $
                            setCardStatus turnedCardId TurnedMatchSucc appState.board,
                          [cardId, turnedCardId]
                        )
                    | cardId == turnedCardId =
                        ( setCardStatus cardId Closed appState.board,
                          [cardId]
                        )
                    | otherwise =
                        ( setCardStatus cardId TurnedMatchFail $
                            setCardStatus turnedCardId TurnedMatchFail appState.board,
                          [cardId, turnedCardId]
                        )
               in appState
                    { gameState = Play (Progress NoCardTurned),
                      board,
                      cardsToRender = appState.cardsToRender <> cardsToRender
                    }
            _ -> appState
       in trace (show newAppState) newAppState
    flipSuccFail :: AppState -> AppState
    flipSuccFail appState =
      let Board cards = appState.board
          newState = map flipState (zip [0 ..] cards)
       in appState
            { board = Board $ map fst newState,
              cardsToRender = mapMaybe snd newState
            }
      where
        flipState :: (CardId, Card) -> (Card, Maybe CardId)
        flipState (cardId, card@Card {..}) =
          let (newCardStatus, cardToRender) = case cardStatus of
                TurnedMatchFail -> (Closed, Just cardId)
                TurnedMatchSucc -> (Turned, Just cardId)
                TurnedWaitPair -> (TurnedWaitPair, Nothing)
                Closed -> (Closed, Nothing)
                Turned -> (Turned, Nothing)
           in (card {cardStatus = newCardStatus}, cardToRender)

    unsafeToInt :: T.Text -> Int
    unsafeToInt t = fromMaybe (error $ "unsafeToInt unable to cast " <> from t) $ readMaybe $ from t

renderApp :: SVGCollections -> TVar AppState -> Logger -> DB.Connection -> IO (Html ())
renderApp cols appStateV _logger _dbConn = do
  appState <- readTVarIO appStateV
  board <- renderBoard cols appStateV
  pure $
    div_ [id_ "AppMain", class_ "flex flex-row justify-center"] $ do
      div_ [class_ "grow min-w-min max-w-screen-2xl"] $ do
        div_ [class_ "flex-col justify-between h-full"] $ do
          div_ [class_ "flex justify-around m-1"] $ do
            button "clickButton" "Menu"
            button "clickRestart" "Restart"
          case appState.gameState of
            Play _ -> do
              div_ [class_ ""] $ do
                board
                renderLeaderBoard
            _ -> p_ "Menu"
          div_ [class_ "bg-indigo-100"] $ do
            p_ "Footer"
  where
    button :: T.Text -> T.Text -> Html ()
    button evText text =
      withEvent evText []
        $ div_
          [ class_
              "px-1 cursor-pointer border-2 rounded border-gray-200 whitespace-nowrap"
          ]
        $ toHtml text

renderBoard :: SVGCollections -> TVar AppState -> IO (Html ())
renderBoard cols appStateV = do
  cards <- mapM (renderCard cols appStateV) [0 .. 23]
  pure $ div_ [id_ "Board", class_ "flex flex-row flex-wrap"] $ do
    div_ [class_ "basis-1/2 min-w-fit grow bg-green-400"] $ do
      div_ [class_ "m-2 grid grid-flow-row-dense gap-2 grid-cols-6 grid-rows-3 justify-items-center"] $ do
        sequence_ cards

renderCard :: SVGCollections -> TVar AppState -> CardId -> IO (Html ())
renderCard cols appStateV cardId = do
  appState <- readTVarIO appStateV
  pure $ div_ [id_ cardDivId] $ do
    case getCardByCardId appState.board cardId of
      Card _ Closed -> div_ [class_ "cursor-pointer"] $ do
        withEvent "clickCard" [cardIdHX] $
          div_ [class_ boxes_style] ""
      Card _ Turned -> do
        div_ [class_ boxes_style] $ img_ [src_ $ getSVGInline appState]
      Card _ TurnedMatchSucc -> do
        div_ [class_ boxes_style] $
          div_ [class_ "bg-green-300"] $
            img_ [src_ $ getSVGInline appState]
      Card _ TurnedMatchFail -> do
        div_ [class_ boxes_style] $
          div_ [class_ "bg-red-300"] $
            img_ [src_ $ getSVGInline appState]
      Card _ TurnedWaitPair -> do
        div_ [class_ boxes_style] $
          div_ [class_ "bg-gray-100"] $
            img_ [src_ $ getSVGInline appState]
  where
    getSVGInline :: AppState -> T.Text
    getSVGInline appState =
      let svgName = getCardName appState.board cardId
          svg = fromMaybe (error "no svg") $ getSVGByName cols appState.collectionName svgName
          svgB64 = B64.encode svg
       in "data:image/svg+xml;base64," <> from (unpack svgB64)
    boxes_style = "w-14 md:w-24 lg:w-32 h-14 md:h-24 lg:h-32 shadow shadow-black bg-yellow-100 border-2 rounded border-pink-100"
    cardIdHX = mkHxVals [("index", T.pack $ show cardId)]
    cardDivId = from $ "Card" <> show cardId

renderLeaderBoard :: Html ()
renderLeaderBoard = do
  div_ [class_ "basis-1/2 min-w-min grow bg-pink-400"] $ do
    p_ "LeaderBoard"
    p_ "Elsa"
    p_ "Fabien"

run :: Int -> IO ()
run port = do
  app <- memoryApp
  runServer port app
