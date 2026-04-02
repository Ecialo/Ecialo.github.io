{-# LANGUAGE CPP #-}

----------------------------------------------------------------------------
module Main where

----------------------------------------------------------------------------

import Lib
import Miso

----------------------------------------------------------------------------

----------------------------------------------------------------------------

-- | Entry point for a miso application
main :: IO ()
#ifdef INTERACTIVE
main = reload defaultEvents app
#else
main = startApp defaultEvents app
#endif

----------------------------------------------------------------------------

-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------