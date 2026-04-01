{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib (runM) where

import Data.Functor.Identity (Identity)
import Data.Int
import Data.Time.Clock
import Miso
import Miso.Lens

data Model
    = Model
    { _counter :: Int
    }
    deriving (Show, Eq)

----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record{_counter = field}

----------------------------------------------------------------------------

-- | Sum type for App events
data Action
    = AddOne
    | SubtractOne
    | SayHelloWorld
    deriving (Show, Eq)

runM :: IO ()
runM = do
    time <- getCurrentTime
    run (startApp (app))

app :: App Model Action
app =
    (component emptyModel updateModel viewModel)
        { styles = [Href "./static/index.css"]
        }

----------------------------------------------------------------------------

-- | Empty application state
emptyModel :: Model
emptyModel = Model 0

----------------------------------------------------------------------------

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel = \case
    AddOne -> counter += 1
    SubtractOne -> counter -= 1
    SayHelloWorld -> io_ $ do
        alert "Hello World"
        consoleLog "Hello World"

----------------------------------------------------------------------------

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel x =
    div_
        []
        [ button_ [onClick AddOne] [text "+"]
        , text $ ms (x ^. counter)
        , button_ [onClick SubtractOne] [text "-"]
        , br_ []
        , button_ [onClick SayHelloWorld] [text "Alert Hello World!"]
        ]
