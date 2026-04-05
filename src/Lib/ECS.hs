{-# LANGUAGE TemplateHaskell #-}

module Lib.ECS where

import Apecs
import Apecs.Physics

makeWorld "World" [''Physics]