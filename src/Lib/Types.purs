module Lib.Types where

import Prelude

type Metric = { surface :: Number, volume :: Number }

calcProportion :: Metric -> Metric -> Metric
calcProportion from to = { surface: to.surface / from.surface, volume: to.volume / from.volume }