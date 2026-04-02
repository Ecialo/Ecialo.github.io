module Lib.Mold where

import Miso.Prelude

data Mold
    = RoundMold {radius :: Double, height :: Double, isOpen :: Bool}
    | RectMold {width :: Double, depth :: Double, height :: Double, isOpen :: Bool}
    deriving (Show, Eq)

computeVolume :: Mold -> Double
computeVolume (RoundMold r h _) = pi * r ^ 2 * h
computeVolume (RectMold w d h _) = w * d * h

computeSurfaceArea :: Mold -> Double
computeSurfaceArea (RoundMold r h isOpen) = 2 * pi * r * (r + h) - if isOpen then pi * r ^ 2 else 0
computeSurfaceArea (RectMold w d h isOpen) = 2 * (w * d + w * h + d * h) - if isOpen then w * d else 0

surfaceCoefficient :: Mold -> Mold -> Double
surfaceCoefficient mold1 mold2 = computeSurfaceArea mold1 / computeSurfaceArea mold2

volumeCoefficient :: Mold -> Mold -> Double
volumeCoefficient mold1 mold2 = computeVolume mold1 / computeVolume mold2