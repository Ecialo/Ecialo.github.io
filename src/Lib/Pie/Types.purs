module Lib.Pie.Types where

import Lib.Recipe
import Lib.Types
import Prelude
import Halogen as H

data KindOfPie = Frozen | Regular
data RecipePart = Crust | Stuff

type PieSlots =
  ( mold :: forall query. H.Slot query Metric Unit
  , recipe :: H.Slot RecipeQuery RecipeOutput String
  )

data PieOutput
  = ChangeMoldMetric Metric
  | ChangeIngredient RecipePart RecipeOutput

data PieAction
  = RaiseMetric Metric
  | RaiseIngredientChange RecipePart RecipeOutput

data PieQuery a
  = AddIngredient RecipePart Ingredient a
  | RemoveIngredient RecipePart String a
