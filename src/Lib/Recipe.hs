module Lib.Recipe where

import Lib.Mold
import Miso.Prelude
import Miso.String (empty)

data Amount = Amount
    { value :: Double
    , unit :: MisoString
    }
    deriving (Show, Eq)

data Ingredient = Ingredient
    { ingredientName :: MisoString
    , ingredientQuantity :: Amount
    }
    deriving (Show, Eq)

data RecipeForm = PieRecipe
    { recipeMold :: Mold
    , recipeCrust :: [Ingredient]
    , recipeFilling :: [Ingredient]
    }
    deriving (Show, Eq)

data Recipe = Recipe
    { recipeName :: MisoString
    , recipeForm :: RecipeForm
    , recipeInstructions :: MisoString
    }
    deriving (Show, Eq)

emptyRecipe :: Recipe
emptyRecipe =
    Recipe
        { recipeName = empty
        , recipeForm =
            PieRecipe
                { recipeMold = Mold{shape = Rect{width = 0, depth = 0, height = 0}, isOpen = False}
                , recipeCrust = []
                , recipeFilling = []
                }
        , recipeInstructions = empty
        }

emptyRecipeForm :: RecipeForm
emptyRecipeForm =
    PieRecipe
        { recipeMold = Mold{shape = Rect{width = 0, depth = 0, height = 0}, isOpen = False}
        , recipeCrust = []
        , recipeFilling = []
        }