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
        { _recipeName = "Beef Pot Pie"
        , _recipeForm =
            PieRecipe
                { _recipeMold = Mold{_shape = Round{_radius = 4.5, _height = 2.5}, _isOpen = False}
                , _recipeCrust =
                    [ Ingredient "All-purpose flour" (Amount 375 Gram)
                    , Ingredient "Salt" (Amount 3 Gram)
                    , Ingredient "Unsalted butter" (Amount 226 Gram)
                    , Ingredient "Cold water" (Amount 118 Ml)
                    , Ingredient "Egg" (Amount 1 Piece)
                    ]
                , _recipeFilling =
                    [ Ingredient "Beef chuck or brisket" (Amount 900 Gram)
                    , Ingredient "Vegetable oil" (Amount 30 Ml)
                    , Ingredient "Butter" (Amount 15 Gram)
                    , Ingredient "Onion" (Amount 1 Piece)
                    , Ingredient "Carrot" (Amount 2 Piece)
                    , Ingredient "Garlic" (Amount 2 Piece)
                    , Ingredient "Tomato paste" (Amount 30 Gram)
                    , Ingredient "Worcestershire sauce" (Amount 30 Ml)
                    , Ingredient "Flour" (Amount 16 Gram)
                    , Ingredient "Dark ale" (Amount 177 Ml)
                    , Ingredient "Beef broth" (Amount 118 Ml)
                    , Ingredient "Thyme" (Amount 1 Gram)
                    , Ingredient "Rosemary" (Amount 1 Gram)
                    , Ingredient "Salt" (Amount 5 Gram)
                    , Ingredient "Ground black pepper" (Amount 1 Gram)
                    ]
                }
        , _recipeInstructions =
            "1. Prepare the crust:\n"
                <> "In a food processor or by hand, combine flour and salt. Add cold butter and cut it in until the mixture resembles coarse crumbs.\n"
                <> "Gradually add cold water one tablespoon at a time until the dough comes together. Divide into 2 portions, wrap, and chill for 30-60 minutes.\n\n"
                <> "2. Prepare the filling:\n"
                <> "Heat the oils, sear the beef in batches until browned, then remove it. In the same pan, add onion and carrot and cook for 5 minutes, then add garlic, tomato paste, and Worcestershire sauce and cook for 1 more minute.\n"
                <> "Add the flour and cook for 2 minutes, then pour in the ale and stir until the sauce thickens. Return the meat, add broth, herbs, salt, and pepper. Simmer covered for 1-1.5 hours until the beef is tender, then cool.\n\n"
                <> "3. Assemble and bake:\n"
                <> "Preheat the oven to 220°C. Grease a 9\" (23-25 cm) pan with oil. Roll out one portion of dough into the pan; you can blind bake it for 10-15 minutes. Add the filling.\n"
                <> "Top with the second dough portion, seal the edges, cut 2-3 vents, and brush with egg wash. Bake for 15-20 minutes if the filling is hot, or 30 minutes if cold, until golden brown.\n\n"
                <> "Tip: serve with mashed potatoes and green peas."
        }
