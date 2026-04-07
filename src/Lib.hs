{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib (makeCalculatorApp, makeGameApp) where

import Apecs
import Apecs.Physics
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Int
import Data.Time.Clock
import Game.Camera (mkCamera)
import Game qualified as G
import Lib.ECS (initWorld)
import Lib.Recipe (emptyRecipe, emptyRecipeForm, testRecipe)
import Miso
import Miso.Html
import Miso.Lens
import Miso.Prelude hiding (global)
import View

makeCalculatorApp :: IO (App Model Action)
makeCalculatorApp = pure $ component (recipeStateFromRecipe testRecipe) updateModel viewModel

-- { styles = [Href "./static/index.css" (True :: CacheBust)]
-- }

makeGameApp :: IO (App G.Model G.Action)
makeGameApp = do
    w <- initWorld
    runWith w $ do
        Apecs.Physics.set Apecs.Physics.global earthGravity
        -- Create an entity with a Position component
        let ballshape = cCircle 1
        ball <- newEntity (DynamicBody, Position (V2 100 100))
        _ <- newEntity (Shape ball ballshape, Density 1)
        pure ()
    let initialCam = mkCamera 0 0 1.0 400 400
    pure $ (component (G.M w 0 initialCam) G.update G.view){subs = [timerSub]}

timerSub :: Sub G.Action
timerSub sink = forever $ (threadDelay 100) >> sink G.Tick