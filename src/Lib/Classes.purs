module Lib.Classes where

import Prelude

import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

cl :: String -> HH.ClassName
cl = HH.ClassName

cls :: forall i r. Array String -> IProp (class ∷ String | r) i
cls c = HP.classes $ map cl c