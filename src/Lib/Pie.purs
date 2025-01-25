module Lib.Pie where

import Data.Maybe
import Lib.Common
import Lib.Mold
import Lib.Pie.Types
import Lib.Recipe
import Lib.Types
import Prelude
import Type.Proxy

import Effect.Class (class MonadEffect)
import Halogen (HalogenF(..))
import Halogen as H
import Halogen.HTML as HH

type Pie =
  { mold :: Mold
  , crustRecipe :: Recipe
  , stuffRecipe :: Recipe
  }

data PieQuery a
  = AddIngredient RecipePart Ingredient a
  | RemoveIngredient RecipePart String a

_pie = Proxy :: Proxy "pie"

type PieSlots =
  ( mold :: forall query. H.Slot query Metric Unit
  , recipe :: forall query. H.Slot query RecipeOutput String
  )

pieComponent :: forall query input m. MonadEffect m => KindOfPie -> H.Component query input PieOutput m
pieComponent pieKind = H.mkComponent
  { initialState: const simplePie
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

  where
  handleAction = case _ of
    RaiseMetric metric -> H.raise (ChangeMoldMetric metric)

  render = case pieKind of
    Frozen -> renderFrozenPie
    Regular -> renderPie

renderPie :: forall m. MonadEffect m => Pie -> H.ComponentHTML PieAction PieSlots m
renderPie _ = HH.div_
  [ HH.h2_ [ HH.text "Mold" ]
  , HH.slot _mold unit moldComponent unit RaiseMetric
  , HH.slot_ _recipe "crust" (recipeComponent) unit
  , HH.slot_ _recipe "stuff" (recipeComponent) unit
  ]

handleQuery :: forall a m s o. PieQuery a -> H.HalogenM Pie PieAction s o m (Maybe a)
handleQuery = case _ of
  AddIngredient part ing a -> do
    -- modify_ $ \pie -> case part of
    --   Crust -> { pie | crustRecipe = addIngredient ing pie.crustRecipe }
    --   Stuff -> { pie | stuffRecipe = addIngredient ing pie.stuffRecipe }
    pure $ Just a
  RemoveIngredient part name a -> do
    -- modify_ $ \pie -> case part of
    --   Crust -> { pie | crustRecipe = removeIngredient name pie.crustRecipe }
    --   Stuff -> { pie | stuffRecipe = removeIngredient name pie.stuffRecipe }
    pure $ Just a

renderFrozenPie :: forall w i. Pie -> HH.HTML w i
renderFrozenPie _ = HH.div_ [ HH.text "Pie" ]

simplePie :: Pie
simplePie = { mold: simpleMold, crustRecipe: newRecipe, stuffRecipe: newRecipe }