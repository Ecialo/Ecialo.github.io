{-# LANGUAGE LambdaCase #-}

module View where

import Lib.Mold
import Lib.Recipe
import Miso
import Miso.Html
import Miso.Html.Property (checked_, class_, name_, placeholder_, type_, value_)
import Miso.Lens
import Miso.Prelude
import Miso.String hiding (map, zipWith)
import View.Types

data Side = L | R
    deriving (Show, Eq)

data Section = Crust | Filling
    deriving (Show, Eq)

data Action
    = SetMold Side Mold
    | UpdateMoldChecked Side Bool
    | UpdateMoldShape Side Shape
    | Recalculate
    | EditIngredientValue Section Int MisoString
    | SetNewIngredientName Section MisoString
    | SetNewIngredientValue Section MisoString
    | SetNewIngredientUnit Section MisoString
    | AddIngredient Section
    | RemoveIngredient Section Int
    | ClearRecipe
    deriving (Show, Eq)

type Model = RecipeState

recipeStateFromRecipe :: Recipe -> RecipeState
recipeStateFromRecipe recipe =
    RecipeState
        { _leftRecipe = recipe
        , _rightRecipe = recipe
        , _surfaceCoef = 1
        , _volumeCoef = 1
        , _newCrust = NewIngredient mempty mempty mempty
        , _newFilling = NewIngredient mempty mempty mempty
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
    EditIngredientValue section idx rawValue -> do
        let val = case fromMisoStringEither rawValue of
                Right num -> num
                Left _ -> 0.0
        let ingLens = case section of
                Crust -> leftRecipe . recipeForm . recipeCrust
                Filling -> leftRecipe . recipeForm . recipeFilling
        ings <- use (this . ingLens)
        this . ingLens .= updateIngredientValueAt idx val ings
        recalc
    SetNewIngredientName section name' -> case section of
        Crust -> this . newCrust . niName .= name'
        Filling -> this . newFilling . niName .= name'
    SetNewIngredientValue section val -> case section of
        Crust -> this . newCrust . niValue .= val
        Filling -> this . newFilling . niValue .= val
    SetNewIngredientUnit section unit' -> case section of
        Crust -> this . newCrust . niUnit .= unit'
        Filling -> this . newFilling . niUnit .= unit'
    AddIngredient section -> do
        let (newLens, ingLens) = case section of
                Crust -> (newCrust, leftRecipe . recipeForm . recipeCrust)
                Filling -> (newFilling, leftRecipe . recipeForm . recipeFilling)
        ni <- use (this . newLens)
        let val = case fromMisoStringEither (ni ^. niValue) of
                Right num -> num
                Left _ -> 0.0
        let ingredient = Ingredient (ni ^. niName) (Amount val (ni ^. niUnit))
        ings <- use (this . ingLens)
        this . ingLens .= ings ++ [ingredient]
        this . newLens .= NewIngredient mempty mempty mempty
        recalc
    RemoveIngredient section idx -> do
        let ingLens = case section of
                Crust -> leftRecipe . recipeForm . recipeCrust
                Filling -> leftRecipe . recipeForm . recipeFilling
        ings <- use (this . ingLens)
        this . ingLens .= removeAt idx ings
        recalc
    ClearRecipe -> do
        leftMold' <- use (this . leftRecipe . recipeForm . recipeMold)
        rightMold' <- use (this . rightRecipe . recipeForm . recipeMold)
        this . leftRecipe .= Recipe{_recipeName = mempty, _recipeForm = PieRecipe{_recipeMold = leftMold', _recipeCrust = [], _recipeFilling = []}, _recipeInstructions = mempty}
        this . rightRecipe .= Recipe{_recipeName = mempty, _recipeForm = PieRecipe{_recipeMold = rightMold', _recipeCrust = [], _recipeFilling = []}, _recipeInstructions = mempty}
        this . newCrust .= NewIngredient mempty mempty mempty
        this . newFilling .= NewIngredient mempty mempty mempty
        recalc
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
viewModel RecipeState{_leftRecipe, _rightRecipe, _surfaceCoef, _volumeCoef, _newCrust, _newFilling} =
    div_ [class_ "calculator"]
        [ h1_ [class_ "title"] [text "Калькулятор рецептов"]
        , div_ [class_ "recipe-name-row"]
            [ h2_ [class_ "recipe-name"] [text (_recipeName _leftRecipe)]
            , button_ [onClick ClearRecipe, class_ "clear-btn"] [text "Очистить"]
            ]
        , div_ [class_ "molds-row"]
            [ viewMoldPanel L "Оригинальная форма" (_recipeMold (_recipeForm _leftRecipe))
            , viewMoldPanel R "Целевая форма" (_recipeMold (_recipeForm _rightRecipe))
            ]
        , div_ [class_ "recipe-row"]
            [ div_ [class_ "recipe-panel"]
                [ h3_ [class_ "panel-header"] [text "Оригинальный"]
                , viewEditableIngredientGroup Crust "Тесто (Crust)" (_recipeCrust (_recipeForm _leftRecipe)) _newCrust
                , viewEditableIngredientGroup Filling "Начинка (Filling)" (_recipeFilling (_recipeForm _leftRecipe)) _newFilling
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
formatCoef x = toMisoString (fromIntegral (round (x * 10) :: Int) / 10)

viewMoldPanel :: Side -> MisoString -> Mold -> View Model Action
viewMoldPanel side lbl Mold{_shape, _isOpen} =
    div_ [class_ "mold-panel"]
        [ h3_ [class_ "mold-title"] [text lbl]
        , moldSelector side
        , isOpenToggle side _isOpen
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

isOpenToggle :: Side -> Bool -> View Model Action
isOpenToggle side isOpen' =
    div_ [class_ "pie-toggle"]
        [ label_ [class_ "pie-toggle-option"]
            [ input_
                [ type_ "radio"
                , name_ (case side of L -> "isOpenL"; R -> "isOpenR")
                , checked_ isOpen'
                , onChecked (\(Checked _) -> UpdateMoldChecked side True)
                ]
            , text " открытый"
            ]
        , label_ [class_ "pie-toggle-option"]
            [ input_
                [ type_ "radio"
                , name_ (case side of L -> "isOpenL"; R -> "isOpenR")
                , checked_ (not isOpen')
                , onChecked (\(Checked _) -> UpdateMoldChecked side False)
                ]
            , text " закрытый"
            ]
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

defaultRound :: Shape
defaultRound = Round{_radius = 10, _height = 5}

defaultRect :: Shape
defaultRect = Rect{_width = 20, _depth = 20, _height = 5}

fromStringToMold :: Side -> MisoString -> Action
fromStringToMold side s = case s of
    "CircleMold" -> SetMold side $ Mold{_shape = defaultRound, _isOpen = False}
    _ -> SetMold side $ Mold{_shape = defaultRect, _isOpen = False}

updateIngredientValueAt :: Int -> Double -> [Ingredient] -> [Ingredient]
updateIngredientValueAt idx val = go 0
  where
    go _ [] = []
    go i (x : xs)
        | i == idx = x{ingredientQuantity = (ingredientQuantity x){value = val}} : xs
        | otherwise = x : go (i + 1) xs

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt idx (x : xs)
    | idx == 0 = xs
    | otherwise = x : removeAt (idx - 1) xs

viewEditableIngredientGroup :: Section -> MisoString -> [Ingredient] -> NewIngredient -> View Model Action
viewEditableIngredientGroup section lbl ingredients newIng =
    div_ [class_ "ingredient-group"]
        [ h4_ [class_ "group-title"] [text lbl]
        , ul_ [class_ "ingredient-list"] $ zipWith (viewEditableIngredient section) [0 ..] ingredients
        , viewAddIngredientForm section newIng
        ]

viewEditableIngredient :: Section -> Int -> Ingredient -> View Model Action
viewEditableIngredient section idx Ingredient{ingredientName, ingredientQuantity} =
    li_ [class_ "ingredient-item"]
        [ button_ [onClick (RemoveIngredient section idx)] [text "\215"]
        , span_ [class_ "ingredient-name"] [text ingredientName]
        , input_
            [ type_ "number"
            , value_ (toMisoString (value ingredientQuantity))
            , onChange (EditIngredientValue section idx)
            ]
        , span_ [class_ "ingredient-unit"] [text (unit ingredientQuantity)]
        ]

viewAddIngredientForm :: Section -> NewIngredient -> View Model Action
viewAddIngredientForm section NewIngredient{_niName, _niValue, _niUnit} =
    div_ [class_ "add-ingredient-form"]
        [ input_
            [ type_ "text"
            , value_ _niName
            , placeholder_ "Название"
            , onChange (SetNewIngredientName section)
            ]
        , input_
            [ type_ "number"
            , value_ _niValue
            , placeholder_ "Кол-во"
            , onChange (SetNewIngredientValue section)
            ]
        , input_
            [ type_ "text"
            , value_ _niUnit
            , placeholder_ "Единица"
            , onChange (SetNewIngredientUnit section)
            ]
        , button_ [onClick (AddIngredient section)] [text "+"]
        ]

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
