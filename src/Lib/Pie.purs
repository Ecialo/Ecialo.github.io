module Lib.Pie where

import Data.Maybe
import Lib.Common
import Lib.Mold
import Lib.Recipe
import Lib.Types
import Prelude
import Type.Proxy

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Halogen (HalogenF(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.ARIA (label)
import Lib.Pie.Types as PT

type Pie =
  { mold :: Mold
  , crustRecipe :: Recipe
  , stuffRecipe :: Recipe
  }

_pie = Proxy :: Proxy "pie"

pieComponent :: forall query input m. MonadEffect m => PT.KindOfPie -> H.Component query input PT.PieOutput m
pieComponent pieKind = H.mkComponent
  { initialState: const simplePie
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

  where
  handleAction = case _ of
    PT.RaiseMetric metric -> H.raise (PT.ChangeMoldMetric metric)
    PT.RaiseIngredientChange part ingCh -> H.raise (PT.ChangeIngredient part ingCh)

  render = case pieKind of
    PT.Frozen -> renderFrozenPie
    PT.Regular -> renderPie

renderPie :: forall m. MonadEffect m => Pie -> H.ComponentHTML PT.PieAction PT.PieSlots m
renderPie _ = HH.div_
  [ HH.h2_ [ HH.text "PIE" ]
  , HH.slot _mold unit moldComponent unit PT.RaiseMetric
  , HH.slot _recipe "crust" (recipeComponent) unit $ PT.RaiseIngredientChange PT.Crust
  , HH.slot _recipe "stuff" (recipeComponent) unit $ PT.RaiseIngredientChange PT.Stuff
  ]

-- routeRecipePart ∷ PT.RecipePart → String
-- routeRecipePart = case _ of
--   PT.Crust -> "crust"
--   PT.Stuff -> "stuff"

-- handleQuery :: forall a m o. MonadEffect m => PT.PieQuery a -> H.HalogenM Pie PT.PieAction PieSlots o m (Maybe a)
-- handleQuery = case _ of
--   PT.AddIngredient part ing next -> do
--     liftEffect $ logShow ing
--     H.tell _recipe (routeRecipePart part) $ UpdateIngridient ing
--     pure $ Just next
--   PT.RemoveIngredient part name next -> do
--     H.tell _recipe (routeRecipePart part) $ RemoveIngridient name
--     pure $ Just next

renderFrozenPie :: forall w i. Pie -> HH.HTML w i
renderFrozenPie _ = HH.div_ [ HH.text "Pie" ]

simplePie :: Pie
simplePie = { mold: simpleMold, crustRecipe: newRecipe, stuffRecipe: newRecipe }