-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
module Main where

-----------------------------------------------------------------------------

import Lib
import Miso
import Miso.CSS (StyleSheet)
import qualified Miso.CSS as CSS
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.Lens
import Miso.String

-----------------------------------------------------------------------------
data Action
    = AddOne
    | SubtractOne
    | SayHelloWorld
    deriving (Show, Eq)

-----------------------------------------------------------------------------
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
-----------------------------------------------------------------------------
-- main :: IO ()
-- #ifdef INTERACTIVE
-- main = reload defaultEvents app
-- #else
-- main = startApp defaultEvents app
-- #endif

main :: IO ()
#ifdef INTERACTIVE
main = do 
    app <- makeCalculatorApp
    reload defaultEvents app
#else
main = do
    app <- makeCalculatorApp
    startApp defaultEvents app
#endif
-----------------------------------------------------------------------------
-- app :: App Int Action
-- app =
--     (component 0 updateModel viewModel)
--         { styles = [Sheet sheet]
--         }

-- -----------------------------------------------------------------------------
-- updateModel :: Action -> Effect parent Int Action
-- updateModel = \case
--     AddOne ->
--         this += 1
--     SubtractOne ->
--         this -= 1
--     SayHelloWorld ->
--         io_ (consoleLog "Hello World!")

-- -----------------------------------------------------------------------------
-- viewModel :: Int -> View Int Action
-- viewModel x =
--     H.div_
--         [P.class_ "counter-container"]
--         [ H.h1_
--             [ P.class_ "counter-title"
--             ]
--             [ "🍜 Miso sampler"
--             ]
--         , H.div_
--             [ P.class_ "counter-display"
--             ]
--             [ text (ms x)
--             ]
--         , H.div_
--             [ P.class_ "buttons-container"
--             ]
--             [ H.button_
--                 [ E.onClick AddOne
--                 , P.class_ "decrement-btn"
--                 ]
--                 [text "+"]
--             , H.button_
--                 [ E.onClick SubtractOne
--                 , P.class_ "increment-btn"
--                 ]
--                 [text "-"]
--             ]
--         ]
