{-# LANGUAGE TemplateHaskell #-}

module View.Types where

import Lib.Recipe
import Miso.Lens.TH
import Miso.Prelude

data RecipeState = RecipeState
    { _leftRecipe :: Recipe
    , _rightRecipe :: Recipe
    }
    deriving (Show, Eq)

makeLenses ''RecipeState
