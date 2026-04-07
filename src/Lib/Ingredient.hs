module Lib.Ingredient where

import Miso.Prelude
import Miso.String (ToMisoString(..))

data Unit = Ml | Gram | Piece
    deriving (Show, Eq, Bounded, Enum)

instance ToMisoString Unit where
    toMisoString Ml = "мл"
    toMisoString Gram = "г"
    toMisoString Piece = "шт"

parseUnit :: MisoString -> Unit
parseUnit "Ml" = Ml
parseUnit "Gram" = Gram
parseUnit _ = Piece
