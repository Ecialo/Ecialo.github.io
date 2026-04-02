module View where

import Lib.Mold
import Lib.Recipe
import Miso
import Miso.Html
import Miso.Html.Property (checked_, type_, value_)
import Miso.Prelude
import Miso.String

data Action
    = SetMold Mold
    | UpdateMoldChecked Bool
    | UpdateMoldShape Shape
type Model = RecipeForm

updateWithRect :: Double -> Double -> Double -> Action
updateWithRect w d h = UpdateMoldShape $ Rect{width = w, depth = d, height = h}

updateWithRound :: Double -> Double -> Action
updateWithRound r h = UpdateMoldShape $ Round{radius = r, height = h}

updateModel :: Action -> Effect parent Model Action
updateModel _ = pure ()

viewModel :: Model -> View Model Action
viewModel = viewRecipe

viewMainBody :: View Model Action
viewMainBody = div_ [] []

viewRecipe :: RecipeForm -> View Model Action
viewRecipe PieRecipe{recipeMold, recipeCrust, recipeFilling} = div_ [] [viewRecipeMold recipeMold]

fromInputToAction :: (Double -> Action) -> MisoString -> Action
fromInputToAction action rawInput = case fromMisoStringEither rawInput of
    Right num -> action num
    Left _ -> action 0

viewRecipeMold :: Mold -> View Model Action
viewRecipeMold Mold{shape, isOpen} =
    div_
        []
        [ div_ [] [moldSelector, viewShape shape]
        , isOpenCheckbox
        ]
  where
    isOpenCheckbox = input_ [type_ "checkbox", onChecked (\(Checked b) -> UpdateMoldChecked b), checked_ isOpen]
viewShape :: Shape -> View Model Action
viewShape (Rect w d h) =
    div_
        []
        [ div_ [] [text "Width", input_ [type_ "number", onChange $ fromInputToAction (\w -> updateWithRect w 0 0)]]
        , div_ [] [text "Depth", input_ [type_ "number", onChange $ fromInputToAction (\d -> updateWithRect 0 d 0)]]
        , div_ [] [text "Height", input_ [type_ "number", onChange $ fromInputToAction (\h -> updateWithRect 0 0 h)]]
        ]
viewShape (Round r h) =
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
    "RectMold" -> SetMold $ Mold{shape = Rect{width = 0, depth = 0, height = 0}, isOpen = False}
    "CircleMold" -> SetMold $ Mold{shape = Round{radius = 0, height = 0}, isOpen = False}
    _ -> SetMold $ Mold{shape = Rect{width = 0, depth = 0, height = 0}, isOpen = False}