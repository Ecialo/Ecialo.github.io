{-# LANGUAGE LambdaCase #-}

module View where

import Lib.Mold
import Lib.Recipe
import Miso
import Miso.Html
import Miso.Html.Property (checked_, type_, value_)
import Miso.Lens
import Miso.Prelude
import Miso.String hiding (map)
import View.Types

data Action
    = SetMold Mold
    | UpdateMoldChecked Bool
    | UpdateMoldShape Shape
type Model = RecipeState

handleAction :: Action -> Effect parent Model Action
handleAction = \case
    SetMold mold -> this . leftRecipe . recipeForm . recipeMold .= mold
    UpdateMoldChecked isOpen' -> this . leftRecipe . recipeForm . recipeMold . isOpen .= isOpen'
    UpdateMoldShape shape' -> this . leftRecipe . recipeForm . recipeMold . shape .= shape'

recipeStateFromRecipe :: Recipe -> RecipeState
recipeStateFromRecipe recipe = RecipeState{_leftRecipe = recipe, _rightRecipe = recipe}

updateWithRect :: Double -> Double -> Double -> Action
updateWithRect w d h = UpdateMoldShape $ Rect{_width = w, _depth = d, _height = h}

updateWithRound :: Double -> Double -> Action
updateWithRound r h = UpdateMoldShape $ Round{_radius = r, _height = h}

updateModel :: Action -> Effect parent Model Action
updateModel _ = pure ()

viewModel :: Model -> View Model Action
viewModel RecipeState{_leftRecipe, _rightRecipe} =
    div_
        []
        [ viewRecipe (_recipeForm _leftRecipe)
        , viewRecipe (_recipeForm _rightRecipe)
        ]

viewMainBody :: View Model Action
viewMainBody = div_ [] []

viewRecipe :: RecipeForm -> View Model Action
viewRecipe PieRecipe{_recipeMold, _recipeCrust, _recipeFilling} =
    div_
        []
        [ viewRecipeMold _recipeMold
        , viewIngredientList _recipeCrust
        , viewIngredientList _recipeFilling
        ]

viewIngredientList :: [Ingredient] -> View Model Action
viewIngredientList ingredients = ul_ [] $ map viewIngredient ingredients

viewIngredient :: Ingredient -> View Model Action
viewIngredient Ingredient{ingredientName, ingredientQuantity} =
    li_
        []
        [ text ingredientName
        , text ": "
        , text (toMisoString ingredientQuantity)
        ]

fromInputToAction :: (Double -> Action) -> MisoString -> Action
fromInputToAction action rawInput = case fromMisoStringEither rawInput of
    Right num -> action num
    Left _ -> action 0

viewRecipeMold :: Mold -> View Model Action
viewRecipeMold Mold{_shape, _isOpen} =
    div_
        []
        [ div_ [] [moldSelector, viewShape _shape]
        , isOpenCheckbox
        ]
  where
    isOpenCheckbox = input_ [type_ "checkbox", onChecked (\(Checked b) -> UpdateMoldChecked b), checked_ _isOpen]
viewShape :: Shape -> View Model Action
viewShape (Rect _w _d _h) =
    div_
        []
        [ div_ [] [text "Width", input_ [type_ "number", onChange $ fromInputToAction (\w -> updateWithRect w 0 0)]]
        , div_ [] [text "Depth", input_ [type_ "number", onChange $ fromInputToAction (\d -> updateWithRect 0 d 0)]]
        , div_ [] [text "Height", input_ [type_ "number", onChange $ fromInputToAction (\h -> updateWithRect 0 0 h)]]
        ]
viewShape (Round _r _h) =
    div_
        []
        [ div_ [] [text "Radius", input_ [type_ "number", onChange $ fromInputToAction (\r -> updateWithRound r 0)]]
        , div_ [] [text "Height", input_ [type_ "number", onChange $ fromInputToAction (\h -> updateWithRound 0 h)]]
        ]

moldSelector :: View Model Action
moldSelector =
    select_
        [onChange fromStringToMold]
        [ option_ [value_ "RectMold"] [text "RectMold"]
        , option_ [value_ "CircleMold"] [text "CircleMold"]
        ]

fromStringToMold :: MisoString -> Action
fromStringToMold s = case s of
    "RectMold" -> SetMold $ Mold{_shape = Rect{_width = 0, _depth = 0, _height = 0}, _isOpen = False}
    "CircleMold" -> SetMold $ Mold{_shape = Round{_radius = 0, _height = 0}, _isOpen = False}
    _ -> SetMold $ Mold{_shape = Rect{_width = 0, _depth = 0, _height = 0}, _isOpen = False}