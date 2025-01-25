module Lib.Pie.Frozen where

import Lib.Mold
import Lib.Pie.Types
import Lib.Recipe
import Prelude
import Type.Proxy

import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH

_frozenPie = Proxy :: Proxy "frozenPie"

type FrozenPieState =
  { crustMultiplier :: Number
  , stuffMultiplier :: Number
  }

frozenPieComponent :: forall query input output m. MonadEffect m => H.Component query input output m
frozenPieComponent = H.mkComponent
  { initialState: const newState
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = \_ -> pure unit
      }
  }

  where
  render state = HH.div_
    [ HH.h2_ [ HH.text "Mold" ]
    , HH.slot_ _mold unit moldComponent unit
    , HH.slot_ _recipe "crust" (recipeComponent) unit
    , HH.slot_ _recipe "stuff" (recipeComponent) unit
    ]

newState :: FrozenPieState
newState = { crustMultiplier: 1.0, stuffMultiplier: 1.0 }
