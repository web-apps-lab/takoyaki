{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use zipWith" #-}

module Apps.Memory.Engine where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.FilePath
  ( takeDirectory,
    takeExtension,
    takeFileName,
  )
import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)
import Witch
import Prelude

data CardStatus
  = Turned
  | TurnedWaitPair
  | TurnedMatchSucc
  | TurnedMatchFail
  | Closed
  deriving (Show, Generic, Eq)

data Card = Card
  { cardName :: SVGName,
    cardStatus :: CardStatus
  }
  deriving (Show, Generic, Eq)

newtype Board = Board [Card] deriving (Show, Generic)

type CardId = Int

data ProgressState = NoCardTurned | OneCardTurned CardId
  deriving (Show, Generic)

data PlayState
  = Wait
  | Win
  | Loose
  | Progress ProgressState
  deriving (Show, Generic)

data GameState = Menu | Play PlayState
  deriving (Show, Generic)

instance Serialise GameState

instance Serialise PlayState

instance Serialise ProgressState

data AppState = AppState
  { collectionName :: CollectionName,
    board :: Board,
    gameState :: GameState,
    cardsToRender :: [CardId]
  }
  deriving (Show, Generic)

instance Serialise AppState

instance Serialise Board

instance Serialise Card

instance Serialise CardStatus

mkBoard :: SVGCollections -> IO (Board, CollectionName)
mkBoard svgsCol = do
  collectionName <- getRandomCollectionName svgsCol
  let colSVGNames = getSVGNames svgsCol collectionName
      cards = case colSVGNames of
        Just xs | length xs >= 12 -> do
          let svgs' = take 12 xs
              svgs = svgs' <> svgs'
          map (`Card` Closed) svgs
        _ -> error $ from collectionName <> " faulty SVG Collection"
  shuffleCards <- shuffleM cards
  pure (Board shuffleCards, collectionName)

getCardName :: Board -> CardId -> SVGName
getCardName board = cardName . getCardByCardId board

setCardStatus :: CardId -> CardStatus -> Board -> Board
setCardStatus cardId cardStatus (Board cards) = do
  let flipCard (i, card) =
        if i == cardId
          then card {cardStatus}
          else card
  Board $ map flipCard $ zip [0 ..] cards

getCardByCardId :: Board -> CardId -> Card
getCardByCardId (Board cards) cardId = cards !! cardId

newtype SVGs = SVGs {unSVGs :: Map.Map SVGName SVG}
  deriving (Show)

type CollectionName = Text

type SVGName = Text

type SVG = ByteString

newtype SVGCollections = SVGCollections {unSVGCollections :: Map.Map CollectionName SVGs}

loadSVG :: IO SVGCollections
loadSVG = do
  let filesList = $(embedDir "src/Apps/Memory/svg")
   in pure $ feedCollections filesList (SVGCollections mempty)
  where
    feedCollections :: [(FilePath, ByteString)] -> SVGCollections -> SVGCollections
    feedCollections paths collections = case paths of
      [] -> collections
      ((path, svgData) : xs)
        | takeExtension path /= ".svg" -> feedCollections xs collections
        | takeExtension path == ".svg" ->
            let collectionName = from $ takeDirectory path
                svgName = from $ takeFileName path
                prevCollection =
                  fromMaybe (SVGs mempty) $
                    Map.lookup collectionName (unSVGCollections collections)
                newCollection = SVGs $ Map.insert svgName svgData (unSVGs prevCollection)
             in feedCollections xs $
                  SVGCollections $
                    Map.insert collectionName newCollection (unSVGCollections collections)
      _ -> collections

getCollectionByName :: SVGCollections -> CollectionName -> Maybe SVGs
getCollectionByName cols colName = do
  Map.lookup colName $ unSVGCollections cols

getSVGByName :: SVGCollections -> CollectionName -> SVGName -> Maybe SVG
getSVGByName cols colName svgName = do
  let svgsM = getCollectionByName cols colName
  case svgsM of
    Just svgs -> Map.lookup svgName $ unSVGs svgs
    Nothing -> Nothing

getSVGNames :: SVGCollections -> CollectionName -> Maybe [SVGName]
getSVGNames cols colName = do
  let svgsM = getCollectionByName cols colName
  Map.keys . unSVGs <$> svgsM

getRandomCollectionName :: SVGCollections -> IO CollectionName
getRandomCollectionName cols = do
  let collectionNames = Map.keys $ unSVGCollections cols
  selected <- randomRIO (0, length collectionNames - 1)
  pure $ collectionNames !! selected