module Main where

import Data.Tuple.Nested
import Lib.Classes
import Lib.Common
import Prelude

import Control.Monad.ST.Internal (new)
import Data.Generic.Rep (from)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen (query)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.VDom.Driver (runUI)
import Lib.Mold (handleAction)
import Lib.Pie as Pie
import Lib.Pie.Frozen (_frozenPie, frozenPieComponent)
import Lib.Pie.Types as PT
import Lib.Recipe as R
import Lib.Types (Metric, calcProportion)

data OutActions
  = Plus
  | Minus
  | Crazy

-- data Tree a = Leaf a | Node (Tree a) (Tree a)

-- v = Node
--   (Leaf 1)
--   ( Node
--       (Leaf 2)
--       (Leaf 3)
--   )

computeCrazy :: Int -> Int
computeCrazy x = result
  where
  ccc = x * 64
  madness = ccc + x
  result = madness * 7

myApp :: forall query input output m. H.Component query input output m
myApp = H.mkComponent
  { initialState: createState
  , eval: H.mkEval $ H.defaultEval
      { handleAction = ourHandleAction
      }
  , render: render
  }
  where
  render state = HH.div_
    [ HH.text "EEEE "
    , HH.text $ show state
    , HH.button [ onClick $ const Plus ] [ HH.text "+" ]
    , HH.button [ onClick $ const Minus ] [ HH.text "-" ]
    , HH.button [ onClick $ const Crazy ] [ HH.text "Crazy" ]
    ]
  createState _input = 0

  ourHandleAction action = case action of
    Plus -> H.modify_ \state -> state + 1
    Minus -> H.modify_ \state -> state - 1
    Crazy -> H.modify_ \state -> computeCrazy state

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI myApp unit body
