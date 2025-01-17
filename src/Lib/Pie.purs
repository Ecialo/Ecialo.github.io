module Lib.Pie where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Lib.Mold (Mold)
import Lib.Recipe (Recipe)

type Pie =
  { mold :: Mold
  , crustRecipe :: Recipe
  , stuffRecipe :: Recipe
  }

pieComponent :: forall query input output m. H.Component query input output m
pieComponent = H.mkComponent
  { initialState: const unit
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = \_ -> pure unit
      }
  }
  where
  render _ = HH.div_ [ HH.text "Pie" ]