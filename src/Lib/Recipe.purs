module Lib.Recipe where

import Data.Maybe
import Prelude

import Data.Map as M
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP

type Ingridient =
  { name :: String
  , amount :: Number
  }

newtype Recipe = Recipe (M.Map String Ingridient)

data RecipeAction = AddNewIngridient
type IngridientForm =
  { name :: String
  , amount :: Number
  }

type RecipeState =
  { recipe :: Recipe
  }

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

  -- renderIngridient :: Ingridient -> HH.HTML w i

  renderNewIngridient :: forall w. HH.HTML w RecipeAction
  renderNewIngridient = HH.div_
    [ HH.input [ HP.type_ InputText, HP.placeholder "Ingridient name" ]
    , HH.input [ HP.type_ InputNumber, HP.placeholder "Amount" ]
    , HH.button [ HE.onClick $ const AddNewIngridient ] [ HH.text "Add" ]
    ]