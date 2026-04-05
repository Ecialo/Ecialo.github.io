{-# LANGUAGE LambdaCase #-}

module Lib.Mold where

import Miso.Lens
import Miso.Prelude

data Shape
    = Round {_radius :: Double, _height :: Double}
    | Rect {_width :: Double, _depth :: Double, _height :: Double}
    deriving (Show, Eq)

radius :: Lens Shape Double
radius = lens _radius $ \s r -> case s of
    Round _ _ -> s{_radius = r}
    rect@(Rect{}) -> rect

height :: Lens Shape Double
height = lens _height $ \s h -> case s of
    Round _ _ -> s{_height = h}
    Rect w d _ -> s{_height = h}

width :: Lens Shape Double
width = lens _width $ \s w -> case s of
    Rect _ d h -> s{_width = w}
    round@(Round{}) -> round

depth :: Lens Shape Double
depth = lens _depth $ \s d -> case s of
    Rect w _ h -> s{_depth = d}
    round@(Round{}) -> round

emptyRound :: Shape
emptyRound = Round{_radius = 0, _height = 0}

emptyRect :: Shape
emptyRect = Rect{_width = 0, _depth = 0, _height = 0}

instance Semigroup Shape where
    (Round{_radius = r1, _height = h1}) <> (Round{_radius = r2, _height = h2}) = Round{_radius = r1 + r2, _height = h1 + h2}
    (Rect{_width = w1, _depth = d1, _height = h1}) <> (Rect{_width = w2, _depth = d2, _height = h2}) = Rect{_width = w1 + w2, _depth = d1 + d2, _height = h1 + h2}
    _ <> _ = error "Cannot combine different shapes"
instance Monoid Shape where
    mempty = Rect 0 0 0

data Mold = Mold {_shape :: Shape, _isOpen :: Bool}
    deriving (Show, Eq)

shape :: Lens Mold Shape
shape = lens _shape $ \s sh -> s{_shape = sh}

isOpen :: Lens Mold Bool
isOpen = lens _isOpen $ \s o -> s{_isOpen = o}

computeVolume :: Mold -> Double
computeVolume (Mold (Round r h) _) = pi * r ^ 2 * h
computeVolume (Mold (Rect w d h) _) = w * d * h

computeSurfaceArea :: Mold -> Double
computeSurfaceArea (Mold (Round r h) isOpen) = 2 * pi * r * (r + h) - if isOpen then pi * r ^ 2 else 0
computeSurfaceArea (Mold (Rect w d h) isOpen) = 2 * (w * d + w * h + d * h) - if isOpen then w * d else 0

surfaceCoefficient :: Mold -> Mold -> Double
surfaceCoefficient mold1 mold2 = computeSurfaceArea mold2 / computeSurfaceArea mold1

volumeCoefficient :: Mold -> Mold -> Double
volumeCoefficient mold1 mold2 = computeVolume mold2 / computeVolume mold1