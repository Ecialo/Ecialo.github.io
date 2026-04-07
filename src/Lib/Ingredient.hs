module Lib.Ingredient where

import Miso.Prelude
import Miso.String (ToMisoString (..))

data Unit = Ml | Gram | Piece
    deriving (Show, Eq, Bounded, Enum)

instance ToMisoString Unit where
    toMisoString Ml = "ml"
    toMisoString Gram = "g"
    toMisoString Piece = "pcs"

parseUnit :: MisoString -> Unit
parseUnit "Ml" = Ml
parseUnit "Gram" = Gram
parseUnit _ = Piece
