module Lib.Pie.Types where

import Lib.Recipe
import Lib.Types
import Prelude

data KindOfPie = Frozen | Regular
data RecipePart = Crust | Stuff

data PieOutput
  = ChangeMoldMetric Metric
  | ChangeIngredient RecipePart RecipeOutput

data PieAction
  = RaiseMetric Metric
  | RaiseIngredientChange RecipePart RecipeOutput

data PieQuery a
  = AddIngredient RecipePart Ingredient a
  | RemoveIngredient RecipePart String a
