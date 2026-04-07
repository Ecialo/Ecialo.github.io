module Lib.Recipe where

import Lib.Ingredient
import Lib.Mold
import Miso.Lens
import Miso.Prelude
import Miso.String (ToMisoString, empty)

data Amount = Amount
    { value :: Int
    , unit :: Unit
    }
    deriving (Show, Eq)

instance ToMisoString Amount where
    toMisoString (Amount v u) = toMisoString v <> " " <> toMisoString u

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

applySurfaceScaling :: Double -> RecipeForm -> RecipeForm
applySurfaceScaling coef recipe =
    recipe{_recipeCrust = applyIngridientsScaling coef (_recipeCrust recipe)}

applyVolumeScaling :: Double -> RecipeForm -> RecipeForm
applyVolumeScaling coef recipe =
    recipe{_recipeFilling = applyIngridientsScaling coef (_recipeFilling recipe)}

applyIngridientsScaling :: Double -> [Ingredient] -> [Ingredient]
applyIngridientsScaling = map . scaleIngredient
  where
    scaleIngredient :: Double -> Ingredient -> Ingredient
    scaleIngredient coef (Ingredient name (Amount val unit)) =
        Ingredient name (Amount (round (fromIntegral val * coef)) unit)

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
        { _recipeMold = Mold{_shape = Rect{_width = 20, _depth = 20, _height = 5}, _isOpen = False}
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
                    [ Ingredient "Мука (универсальная пшеничная)" (Amount 375 Gram)
                    , Ingredient "Соль" (Amount 3 Gram)
                    , Ingredient "Сливочное масло" (Amount 226 Gram)
                    , Ingredient "Холодная вода" (Amount 118 Ml)
                    , Ingredient "Яйцо" (Amount 1 Piece)
                    ]
                , _recipeFilling =
                    [ Ingredient "Говядина (лопатка или грудинка)" (Amount 900 Gram)
                    , Ingredient "Растительное масло" (Amount 30 Ml)
                    , Ingredient "Сливочное масло" (Amount 15 Gram)
                    , Ingredient "Лук" (Amount 1 Piece)
                    , Ingredient "Морковь" (Amount 2 Piece)
                    , Ingredient "Чеснок" (Amount 2 Piece)
                    , Ingredient "Томатная паста" (Amount 30 Gram)
                    , Ingredient "Вустерширский соус" (Amount 30 Ml)
                    , Ingredient "Мука" (Amount 16 Gram)
                    , Ingredient "Темный эль" (Amount 177 Ml)
                    , Ingredient "Говяжий бульон" (Amount 118 Ml)
                    , Ingredient "Тимьян" (Amount 1 Gram)
                    , Ingredient "Розмарин" (Amount 1 Gram)
                    , Ingredient "Соль" (Amount 5 Gram)
                    , Ingredient "Молотый черный перец" (Amount 1 Gram)
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
