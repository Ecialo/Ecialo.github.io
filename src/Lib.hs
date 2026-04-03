{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib (app) where

import Data.Int
import Data.Time.Clock
import Lib.Recipe (emptyRecipe, emptyRecipeForm, testRecipe)
import Miso
import Miso.Html
import Miso.Lens
import View

app :: App Model Action
app = component (recipeStateFromRecipe testRecipe) updateModel viewModel

-- { styles = [Href "./static/index.css" (True :: CacheBust)]
-- }
