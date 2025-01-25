module Lib.Recipe where

import Data.Maybe
import Prelude
import Type.Proxy

import Data.Array as A
import Data.List (toUnfoldable)
import Data.List as L
import Data.Map as M
import Data.Number (fromString)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML (li)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP

_recipe = Proxy :: Proxy "recipe"
data FormField = IngredientName | IngredientAmount

data RecipeQuery a
  = UpdateIngridient Ingredient a
  | RemoveIngridient String a
  | SetMultiplier Number a

type Ingredient =
  { name :: String
  , amount :: Number
  }

newtype Recipe = Recipe (M.Map String Ingredient)

data RecipeAction
  = AddNewIngredient
  | RemoveIngredient String
  | UpdateForm FormField String
  | ChangeIngredientAmount String String

data RecipeOutput
  = IngredientRemoved String
  | IngredientUpdated Ingredient

type IngredientForm =
  { name :: String
  , amount :: Number
  }

type RecipeState =
  { recipe :: Recipe
  , form :: IngredientForm
  , multiplier :: Number
  }

withParsedNumber :: forall a. String -> (Number -> a) -> a
withParsedNumber str f = case fromString str of
  Just n -> f n
  Nothing -> f 0.0

newRecipe :: Recipe
newRecipe = Recipe M.empty

getIngredient :: String -> Recipe -> Maybe Ingredient
getIngredient name (Recipe recipe) = M.lookup name recipe

addIngredient :: Ingredient -> Recipe -> Recipe
addIngredient ing (Recipe recipe) = Recipe $ M.insert ing.name ing recipe

removeIngridient :: String -> Recipe -> Recipe
removeIngridient name (Recipe recipe) = Recipe $ M.delete name recipe

changeAmount :: String -> Number -> Recipe -> Recipe
changeAmount name amount (Recipe recipe) = Recipe $ M.update (\ing -> Just $ ing { amount = amount }) name recipe

multiplyAmount :: Number -> Recipe -> Recipe
multiplyAmount n (Recipe recipe) = Recipe $ M.mapMaybe (\ing -> Just ing { amount = ing.amount * n }) recipe

recipeComponent :: forall input m. MonadEffect m => H.Component RecipeQuery input RecipeOutput m
recipeComponent = H.mkComponent
  { initialState: const newRecipeState
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  listOfIngridients mult (Recipe recipe) = HH.ul_ $
    map (\i -> HH.li_ [ renderIngredient mult i ]) (M.values recipe # A.fromFoldable)
  render state = HH.div_ $
    [ HH.text "Recipe"
    , listOfIngridients state.multiplier state.recipe
    , renderNewIngridient
    ]

renderNewIngridient :: forall w. HH.HTML w RecipeAction
renderNewIngridient = HH.div_
  [ HH.input [ HP.type_ InputText, HP.placeholder "Ingridient name", HE.onValueInput \i -> UpdateForm IngredientName i ]
  , HH.input [ HP.type_ InputNumber, HP.placeholder "Amount", HE.onValueInput \i -> UpdateForm IngredientAmount i ]
  , HH.button [ HE.onClick $ const AddNewIngredient ] [ HH.text "+" ]
  ]

renderIngredient :: forall w. Number -> Ingredient -> HH.HTML w RecipeAction
renderIngredient mult ing = HH.div_
  [ HH.text ing.name
  , HH.input [ HP.type_ InputNumber, HP.value $ show $ ing.amount * mult, HE.onValueInput \i -> ChangeIngredientAmount ing.name i ]
  , HH.button [ HE.onClick $ const $ RemoveIngredient ing.name ] [ HH.text "x" ]
  ]

handleAction :: forall m s. RecipeAction -> H.HalogenM RecipeState RecipeAction s RecipeOutput m Unit
handleAction = case _ of
  AddNewIngredient -> do
    state <- H.get
    let newIngredient = { name: state.form.name, amount: state.form.amount }
    H.put $ state { recipe = addIngredient newIngredient state.recipe }
    H.raise $ IngredientUpdated newIngredient
  RemoveIngredient name -> do
    H.modify_ $ \state -> state { recipe = removeIngridient name state.recipe }
    H.raise $ IngredientRemoved name
  UpdateForm IngredientName name -> do
    H.modify_ $ \state -> state { form = state.form { name = name } }
  UpdateForm IngredientAmount amount -> withParsedNumber amount \val ->
    H.modify_ $ \state -> state { form = state.form { amount = val } }
  ChangeIngredientAmount name amount -> withParsedNumber amount \val -> do
    H.modify_ $ \state -> state { recipe = changeAmount name val state.recipe }
    ingredient <- H.gets $ \state -> fromMaybe { name: "", amount: 0.0 } $ getIngredient name state.recipe
    H.raise $ IngredientUpdated ingredient

handleQuery :: forall a m s. MonadEffect m => RecipeQuery a -> H.HalogenM RecipeState RecipeAction s RecipeOutput m (Maybe a)
handleQuery = case _ of
  UpdateIngridient ingredient next -> do
    liftEffect $ logShow ingredient
    H.modify_ $ \state -> state { recipe = addIngredient ingredient state.recipe }
    pure $ Just next
  RemoveIngridient name next -> do
    H.modify_ $ \state -> state { recipe = removeIngridient name state.recipe }
    pure $ Just next
  SetMultiplier mult next -> do
    H.modify_ $ \state -> state { multiplier = mult }
    pure $ Just next

newRecipeState :: RecipeState
newRecipeState = { recipe: newRecipe, form: { name: "", amount: 0.0 }, multiplier: 1.0 }
