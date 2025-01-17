module Lib.Recipe where

import Data.Maybe
import Prelude

import Data.Map as M
import Halogen as H
import Halogen.HTML as HH

type Ingridient =
  { name :: String
  , amount :: Number
  }

newtype Recipe = Recipe (M.Map String Ingridient)

newRecipe :: Recipe
newRecipe = Recipe M.empty

addIngridient :: Ingridient -> Recipe -> Recipe
addIngridient ing (Recipe recipe) = Recipe $ M.insert ing.name ing recipe

removeIngridient :: String -> Recipe -> Recipe
removeIngridient name (Recipe recipe) = Recipe $ M.delete name recipe

changeAmount :: String -> Number -> Recipe -> Recipe
changeAmount name amount (Recipe recipe) = Recipe $ M.update (\ing -> Just $ ing { amount = amount }) name recipe

multiplyAmount :: Number -> Recipe -> Recipe
multiplyAmount n (Recipe recipe) = Recipe $ M.mapMaybe (\ing -> Just ing { amount = ing.amount * n }) recipe

recipeComponent :: forall query input output m. H.Component query input output m
recipeComponent = H.mkComponent
  { initialState: const newRecipe
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = \_ -> pure unit
      }
  }
  where
  render _ = HH.div_ [ HH.text "Recipe" ]