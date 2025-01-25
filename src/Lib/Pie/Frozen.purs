module Lib.Pie.Frozen where

import Data.Maybe
import Lib.Mold
import Lib.Recipe
import Prelude
import Type.Proxy

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH
import Lib.Pie.Types as PT

_frozenPie = Proxy :: Proxy "frozenPie"

type FrozenPieState =
  { crustMultiplier :: Number
  , stuffMultiplier :: Number
  }

frozenPieComponent :: forall input m. MonadEffect m => H.Component PT.PieQuery input PT.PieOutput m
frozenPieComponent = H.mkComponent
  { initialState: const newState
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

  where
  handleAction = case _ of
    PT.RaiseMetric metric -> H.raise (PT.ChangeMoldMetric metric)
    _ -> pure unit

  render state = HH.div_
    [ HH.h2_ [ HH.text "Mold" ]
    , HH.slot _mold unit moldComponent unit PT.RaiseMetric
    , HH.slot_ _recipe "crust" (recipeComponent) unit
    , HH.slot_ _recipe "stuff" (recipeComponent) unit
    ]

newState :: FrozenPieState
newState = { crustMultiplier: 1.0, stuffMultiplier: 1.0 }

routeRecipePart ∷ PT.RecipePart → String
routeRecipePart = case _ of
  PT.Crust -> "crust"
  PT.Stuff -> "stuff"

handleQuery :: forall a m o. MonadEffect m => PT.PieQuery a -> H.HalogenM FrozenPieState PT.PieAction PT.PieSlots o m (Maybe a)
handleQuery = case _ of
  PT.AddIngredient part ing next -> do
    -- liftEffect $ logShow ing
    -- liftEffect
    H.tell _recipe (routeRecipePart part) $ UpdateIngridient ing
    pure $ Just next
  PT.RemoveIngredient part name next -> do
    H.tell _recipe (routeRecipePart part) $ RemoveIngridient name
    pure $ Just next