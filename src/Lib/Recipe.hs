module Lib.Recipe where

import Lib.Mold
import Miso.Lens
import Miso.Prelude
import Miso.String (ToMisoString, empty)

data Amount = Amount
    { value :: Double
    , unit :: MisoString
    }
    deriving (Show, Eq)

instance ToMisoString Amount where
    toMisoString (Amount v u) = toMisoString v <> " " <> u

data Ingredient = Ingredient
    { ingredientName :: MisoString
    , ingredientQuantity :: Amount
    }
    deriving (Show, Eq)

data RecipeForm = PieRecipe
    { _recipeMold :: Mold
    , _recipeCrust :: [Ingredient]
    , _recipeFilling :: [Ingredient]
    }
    deriving (Show, Eq)

recipeMold :: Lens RecipeForm Mold
recipeMold = lens _recipeMold $ \s m -> s{_recipeMold = m}

recipeCrust :: Lens RecipeForm [Ingredient]
recipeCrust = lens _recipeCrust $ \s c -> s{_recipeCrust = c}

recipeFilling :: Lens RecipeForm [Ingredient]
recipeFilling = lens _recipeFilling $ \s f -> s{_recipeFilling = f}

data Recipe = Recipe
    { _recipeName :: MisoString
    , _recipeForm :: RecipeForm
    , _recipeInstructions :: MisoString
    }
    deriving (Show, Eq)

emptyRecipe :: Recipe
emptyRecipe =
    Recipe
        { _recipeName = empty
        , _recipeForm = emptyRecipeForm
        , _recipeInstructions = empty
        }

recipeName :: Lens Recipe MisoString
recipeName = lens _recipeName $ \s n -> s{_recipeName = n}

recipeForm :: Lens Recipe RecipeForm
recipeForm = lens _recipeForm $ \s f -> s{_recipeForm = f}

recipeInstructions :: Lens Recipe MisoString
recipeInstructions = lens _recipeInstructions $ \s i -> s{_recipeInstructions = i}

emptyRecipeForm :: RecipeForm
emptyRecipeForm =
    PieRecipe
        { _recipeMold = Mold{_shape = Rect{_width = 0, _depth = 0, _height = 0}, _isOpen = False}
        , _recipeCrust = []
        , _recipeFilling = []
        }

testRecipe :: Recipe
testRecipe =
    Recipe
        { _recipeName = "Мясной пирог с корочкой"
        , _recipeForm =
            PieRecipe
                { _recipeMold = Mold{_shape = Round{_radius = 4.5, _height = 2.5}, _isOpen = False}
                , _recipeCrust =
                    [ Ingredient "Мука (универсальная пшеничная)" (Amount 3 "стакана (375 г)")
                    , Ingredient "Соль" (Amount 0.5 "чайной ложки")
                    , Ingredient "Сливочное масло" (Amount 1 "стакан (226 г), нарезанное и сильно охлажденное")
                    , Ingredient "Холодная вода" (Amount 0.5 "стакана (118 мл)")
                    , Ingredient "Яйцо" (Amount 1 "шт (взбитое, для смазывания)")
                    ]
                , _recipeFilling =
                    [ Ingredient "Говядина (лопатка или грудинка)" (Amount 900 "г")
                    , Ingredient "Растительное масло" (Amount 2 "ст. ложки")
                    , Ingredient "Сливочное масло" (Amount 1 "ст. ложка")
                    , Ingredient "Лук" (Amount 1 "средняя")
                    , Ingredient "Морковь" (Amount 2 "крупные")
                    , Ingredient "Чеснок" (Amount 2 "зубчика")
                    , Ingredient "Томатная паста" (Amount 2 "ст. ложки")
                    , Ingredient "Вустерширский соус" (Amount 2 "ст. ложки")
                    , Ingredient "Мука" (Amount 2 "ст. ложки")
                    , Ingredient "Темный эль" (Amount 0.75 "стакана (177 мл)")
                    , Ingredient "Говяжий бульон" (Amount 0.5 "стакана (118 мл)")
                    , Ingredient "Тимьян" (Amount 0.5 "чайной ложки (свежий, мелко нарезанный)")
                    , Ingredient "Розмарин" (Amount 0.5 "чайной ложки (свежий, мелко нарезанный)")
                    , Ingredient "Соль" (Amount 1 "чайная ложка")
                    , Ingredient "Молотый черный перец" (Amount 0.25 "чайной ложки")
                    ]
                }
        , _recipeInstructions =
            "1. Подготовьте тесто:\n"
                <> "В кухонном комбайне или вручную смешайте муку и соль. Добавьте холодное сливочное масло, разотрите до крошки.\n"
                <> "Постепенно добавляйте холодную воду по одной ложке, пока тесто не соберется. Разделите на 2 части, заверните и охладите 30-60 мин.\n\n"
                <> "2. Приготовьте начинку:\n"
                <> "Разогрейте масла, обжарьте говядину порциями до золотистой корочки, уберите. В ту же сковороду добавьте лук и морковь, жарьте 5 мин, затем чеснок, томатную пасту и вустерширский соус 1 мин.\n"
                <> "Добавьте муку, жарьте 2 мин, влейте эль, размешайте до загустения. Верните мясо, добавьте бульон, травы, соль, перец. Томите под крышкой 1-1.5 часа до мягкости, остудите.\n\n"
                <> "3. Сборка и выпечка:\n"
                <> "Разогрейте духовку до 220°C. Смажьте форму (9\" / 23-25 см) маслом. Раскатайте одну часть теста в форму, можно выпечь вслепую 10-15 мин. Выложите начинку.\n"
                <> "Накройте второй частью теста, запечатайте края, сделайте 2-3 прореза, смажьте яйцом. Выпекайте 15-20 мин (если начинка горячая) или 30 мин (если холодная) до золотистой корочки.\n\n"
                <> "Совет: подавайте с картофельным пюре и зеленым горошком."
        }
