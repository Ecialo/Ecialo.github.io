{-# LANGUAGE TemplateHaskell #-}

module View.Types where

import Lib.Recipe
import Miso.Lens.TH
import Miso.Prelude

data NewIngredient = NewIngredient
    { _niName :: MisoString
    , _niValue :: MisoString
    , _niUnit :: MisoString
    }
    deriving (Show, Eq)

makeLenses ''NewIngredient

data RecipeState = RecipeState
    { _leftRecipe :: Recipe
    , _rightRecipe :: Recipe
    , _surfaceCoef :: Double
    , _volumeCoef :: Double
    , _newCrust :: NewIngredient
    , _newFilling :: NewIngredient
    }
    deriving (Show, Eq)

makeLenses ''RecipeState
