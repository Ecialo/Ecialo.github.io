module Lib.Mold where

import Prelude
import Data.Number
import Data.Tuple

data MoldForm
  = RoundMold Number Number
  | RectMold Number Number Number

type Mold =
  { form :: MoldForm
  , isClosed :: Boolean
  }

calcVolume :: Mold -> Number
calcVolume mold = case mold.form of
  RoundMold r h -> pi * r * r * h
  RectMold w l h -> w * l * h

calcSurface :: Mold -> Number
calcSurface mold = surface
  where
  closedSurface = case mold.form of
    RoundMold r h -> 2.0 * pi * r * (r + h)
    RectMold w l h -> 2.0 * (w * l + w * h + l * h)
  cap = case mold.form of
    RoundMold r _ -> pi * r * r
    RectMold w l _ -> w * l
  surface = if mold.isClosed then closedSurface else closedSurface - cap