module Lib.Mold where

import Miso.Prelude

data Shape
    = Round {radius :: Double, height :: Double}
    | Rect {width :: Double, depth :: Double, height :: Double}
    deriving (Show, Eq)

emptyRound :: Shape
emptyRound = Round{radius = 0, height = 0}

emptyRect :: Shape
emptyRect = Rect{width = 0, depth = 0, height = 0}

instance Semigroup Shape where
    (Round{radius = r1, height = h1}) <> (Round{radius = r2, height = h2}) = Round{radius = r1 + r2, height = h1 + h2}
    (Rect{width = w1, depth = d1, height = h1}) <> (Rect{width = w2, depth = d2, height = h2}) = Rect{width = w1 + w2, depth = d1 + d2, height = h1 + h2}
    _ <> _ = error "Cannot combine different shapes"
instance Monoid Shape where
    mempty = Rect 0 0 0

data Mold = Mold {shape :: Shape, isOpen :: Bool}
    deriving (Show, Eq)

computeVolume :: Mold -> Double
computeVolume (Mold (Round r h) _) = pi * r ^ 2 * h
computeVolume (Mold (Rect w d h) _) = w * d * h

computeSurfaceArea :: Mold -> Double
computeSurfaceArea (Mold (Round r h) isOpen) = 2 * pi * r * (r + h) - if isOpen then pi * r ^ 2 else 0
computeSurfaceArea (Mold (Rect w d h) isOpen) = 2 * (w * d + w * h + d * h) - if isOpen then w * d else 0

surfaceCoefficient :: Mold -> Mold -> Double
surfaceCoefficient mold1 mold2 = computeSurfaceArea mold1 / computeSurfaceArea mold2

volumeCoefficient :: Mold -> Mold -> Double
volumeCoefficient mold1 mold2 = computeVolume mold1 / computeVolume mold2