module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen (query)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Lib.Recipe as Recipe

type Slots =
  ( fromPie :: forall query output. H.Slot query output Unit
  , toPie :: forall query output. H.Slot query output Unit
  )

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI myApp unit body

myApp :: forall query input output m. H.Component query input output m
myApp = H.mkComponent
  { initialState: const unit
  , eval: H.mkEval $ H.defaultEval
      { handleAction = \_ -> pure unit
      }
  , render: render
  }
  where
  render _ = HH.div_ [ HH.text "Pie" ]
-- render _ = HH.div_
--   [ HH.slot_ Benefit._skillTree unit Benefit.skillTree treeShape
--   , HH.slot_ Benefit._skillTree unit Benefit.skillTree treeShape
--   ]