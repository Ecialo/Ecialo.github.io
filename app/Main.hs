-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
module Main where

-----------------------------------------------------------------------------

import Lib
import Miso

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

main :: IO ()
#ifdef INTERACTIVE
main = do
    app <- makeCalculatorApp
    -- css <- readFile "./static/index.css"
    -- reload defaultEvents (app {styles = [Style (ms css)]})
    -- p <- HomepagePaths.getDataFileName "static/index.css"
    
    reload defaultEvents (app {styles = [Href ("https://raw.githubusercontent.com/Ecialo/Ecialo.github.io/refs/heads/miso/static/index.css") (False :: CacheBust)]})
#else
main = do
    app <- makeCalculatorApp
    startApp defaultEvents app
#endif
