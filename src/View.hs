{-# LANGUAGE LambdaCase #-}

module View where

import Lib.Mold
import Lib.Recipe
import Miso
import Miso.Html
import Miso.Html.Property (checked_, class_, type_, value_)
import Miso.Lens
import Miso.Prelude
import Miso.String hiding (map)
import View.Types

data Side = L | R
    deriving (Show, Eq)

data Action
    = SetMold Side Mold
    | UpdateMoldChecked Side Bool
    | UpdateMoldShape Side Shape
    | Recalculate
    deriving (Show, Eq)

type Model = RecipeState

recipeStateFromRecipe :: Recipe -> RecipeState
recipeStateFromRecipe recipe =
    RecipeState
        { _leftRecipe = recipe
        , _rightRecipe = recipe
        , _surfaceCoef = 1
        , _volumeCoef = 1
        }

updateWithRect :: Side -> Double -> Double -> Double -> Action
updateWithRect side w d h = UpdateMoldShape side $ Rect{_width = w, _depth = d, _height = h}

updateWithRound :: Side -> Double -> Double -> Action
updateWithRound side r h = UpdateMoldShape side $ Round{_radius = r, _height = h}

updateModel :: Action -> Effect parent Model Action
updateModel = \case
    SetMold side mold -> do
        case side of
            L -> this . leftRecipe . recipeForm . recipeMold .= mold
            R -> this . rightRecipe . recipeForm . recipeMold .= mold
        recalc
    UpdateMoldChecked side isOpen' -> do
        case side of
            L -> this . leftRecipe . recipeForm . recipeMold . isOpen .= isOpen'
            R -> this . rightRecipe . recipeForm . recipeMold . isOpen .= isOpen'
        recalc
    UpdateMoldShape side shape' -> do
        case side of
            L -> this . leftRecipe . recipeForm . recipeMold . shape .= shape'
            R -> this . rightRecipe . recipeForm . recipeMold . shape .= shape'
        recalc
    Recalculate -> recalc
  where
    recalc = do
        leftCrust <- use (this . leftRecipe . recipeForm . recipeCrust)
        leftFilling <- use (this . leftRecipe . recipeForm . recipeFilling)
        leftMold' <- use (this . leftRecipe . recipeForm . recipeMold)
        rightMold' <- use (this . rightRecipe . recipeForm . recipeMold)
        let surfaceCoef' = surfaceCoefficient leftMold' rightMold'
            volumeCoef' = volumeCoefficient leftMold' rightMold'
        this . surfaceCoef .= surfaceCoef'
        this . volumeCoef .= volumeCoef'
        this . rightRecipe . recipeForm . recipeCrust .= applyIngridientsScaling surfaceCoef' leftCrust
        this . rightRecipe . recipeForm . recipeFilling .= applyIngridientsScaling volumeCoef' leftFilling

viewModel :: Model -> View Model Action
viewModel RecipeState{_leftRecipe, _rightRecipe, _surfaceCoef, _volumeCoef} =
    div_ [class_ "calculator"]
        [ h1_ [class_ "title"] [text "Калькулятор рецептов"]
        , h2_ [class_ "recipe-name"] [text (_recipeName _leftRecipe)]
        , div_ [class_ "molds-row"]
            [ viewMoldPanel L "Оригинальная форма" (_recipeMold (_recipeForm _leftRecipe))
            , viewMoldPanel R "Целевая форма" (_recipeMold (_recipeForm _rightRecipe))
            ]
        , div_ [class_ "recipe-row"]
            [ div_ [class_ "recipe-panel"]
                [ h3_ [class_ "panel-header"] [text "Оригинальный"]
                , viewIngredientGroup "Тесто (Crust)" (_recipeCrust (_recipeForm _leftRecipe))
                , viewIngredientGroup "Начинка (Filling)" (_recipeFilling (_recipeForm _leftRecipe))
                ]
            , div_ [class_ "arrows"]
                [ div_ [class_ "arrow"]
                    [ text "Crust \8594 \215"
                    , text (formatCoef _surfaceCoef)
                    ]
                , div_ [class_ "arrow"]
                    [ text "Filling \8594 \215"
                    , text (formatCoef _volumeCoef)
                    ]
                ]
            , div_ [class_ "recipe-panel"]
                [ h3_ [class_ "panel-header"] [text "Пересчитанный"]
                , viewIngredientGroup "Тесто (Crust)" (_recipeCrust (_recipeForm _rightRecipe))
                , viewIngredientGroup "Начинка (Filling)" (_recipeFilling (_recipeForm _rightRecipe))
                ]
            ]
        , viewInstructions (_recipeInstructions _leftRecipe)
        ]

formatCoef :: Double -> MisoString
formatCoef = toMisoString

viewMoldPanel :: Side -> MisoString -> Mold -> View Model Action
viewMoldPanel side lbl Mold{_shape, _isOpen} =
    div_ [class_ "mold-panel"]
        [ h3_ [class_ "mold-title"] [text lbl]
        , moldSelector side
        , isOpenCheckbox side _isOpen
        , viewShape side _shape
        ]

moldSelector :: Side -> View Model Action
moldSelector side =
    div_ [class_ "mold-controls"]
        [ text "Форма: "
        , select_
            [onChange (fromStringToMold side)]
            [ option_ [value_ "CircleMold"] [text "Круглая"]
            , option_ [value_ "RectMold"] [text "Прямоугольная"]
            ]
        ]

isOpenCheckbox :: Side -> Bool -> View Model Action
isOpenCheckbox side isChecked =
    label_ [class_ "mold-controls"]
        [ input_
            [ type_ "checkbox"
            , checked_ isChecked
            , onChecked (\(Checked b) -> UpdateMoldChecked side b)
            ]
        , text " Открытая форма"
        ]

viewShape :: Side -> Shape -> View Model Action
viewShape side (Rect w d h) =
    div_ [class_ "shape-inputs"]
        [ labeledInput "Ширина" w (\v -> updateWithRect side v d h)
        , labeledInput "Глубина" d (\v -> updateWithRect side w v h)
        , labeledInput "Высота" h (\v -> updateWithRect side w d v)
        ]
viewShape side (Round r h) =
    div_ [class_ "shape-inputs"]
        [ labeledInput "Радиус" r (\v -> updateWithRound side v h)
        , labeledInput "Высота" h (\v -> updateWithRound side r v)
        ]

labeledInput :: MisoString -> Double -> (Double -> Action) -> View Model Action
labeledInput lbl val action =
    div_ [class_ "input-row"]
        [ text lbl
        , input_
            [ type_ "number"
            , value_ (toMisoString val)
            , onChange (fromInputToAction action)
            ]
        ]

fromInputToAction :: (Double -> Action) -> MisoString -> Action
fromInputToAction action rawInput = case fromMisoStringEither rawInput of
    Right num -> action num
    Left _ -> action 0

fromStringToMold :: Side -> MisoString -> Action
fromStringToMold side s = case s of
    "CircleMold" -> SetMold side $ Mold{_shape = Round{_radius = 0, _height = 0}, _isOpen = False}
    "RectMold" -> SetMold side $ Mold{_shape = Rect{_width = 0, _depth = 0, _height = 0}, _isOpen = False}
    _ -> SetMold side $ Mold{_shape = Rect{_width = 0, _depth = 0, _height = 0}, _isOpen = False}

viewIngredientGroup :: MisoString -> [Ingredient] -> View Model Action
viewIngredientGroup lbl ingredients =
    div_ [class_ "ingredient-group"]
        [ h4_ [class_ "group-title"] [text lbl]
        , ul_ [class_ "ingredient-list"] $ map viewIngredient ingredients
        ]

viewIngredient :: Ingredient -> View Model Action
viewIngredient Ingredient{ingredientName, ingredientQuantity} =
    li_ [class_ "ingredient-item"]
        [ span_ [class_ "ingredient-name"] [text ingredientName]
        , span_ [class_ "ingredient-quantity"] [text (toMisoString ingredientQuantity)]
        ]

viewInstructions :: MisoString -> View Model Action
viewInstructions instr =
    div_ [class_ "instructions"]
        [ h3_ [class_ "panel-header"] [text "Инструкция"]
        , div_ [class_ "instructions-text"] [text instr]
        ]
