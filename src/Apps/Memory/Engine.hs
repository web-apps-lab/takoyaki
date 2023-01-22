module Apps.Memory.Engine where

import Codec.Serialise (Serialise)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

newtype AppState = AppState
  { seed :: Text
  }
  deriving (Show, Generic)

instance Serialise AppState
