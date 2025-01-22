module Lib.Pie where

import Prelude
import Type.Proxy

import Halogen as H
import Halogen.HTML as HH
import Lib.Mold
import Lib.Recipe (Recipe, newRecipe)
import Lib.Common

type Pie =
  { mold :: Mold
  , crustRecipe :: Recipe
  , stuffRecipe :: Recipe
  }

_pie = Proxy :: Proxy "pie"

type PieSlots = (mold :: forall query. H.Slot query Void Unit)

pieComponent :: forall query input output m. KindOfPie -> H.Component query input output m
pieComponent pieKind = H.mkComponent
  { initialState: const simplePie
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = \_ -> pure unit
      }
  }

  where
  render = case pieKind of
    Frozen -> renderFrozenPie
    Regular -> renderPie

renderPie :: forall action m. Pie -> H.ComponentHTML action PieSlots m
renderPie _ = HH.div_
  [ HH.div_
      [ HH.h2_ [ HH.text "Mold" ]
      , HH.slot_ _mold unit moldComponent unit
      ]
  ]

renderFrozenPie :: forall w i. Pie -> HH.HTML w i
renderFrozenPie _ = HH.div_ [ HH.text "Pie" ]

simplePie :: Pie
simplePie = { mold: simpleMold, crustRecipe: newRecipe, stuffRecipe: newRecipe }