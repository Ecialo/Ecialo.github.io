module Main where

import Data.Tuple.Nested
import Lib.Classes
import Lib.Common
import Prelude

import Data.Generic.Rep (from)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen (query)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Lib.Pie as Pie
import Lib.Pie.Frozen (_frozenPie, frozenPieComponent)
import Lib.Pie.Types as PT
import Lib.Recipe as R
import Lib.Types (Metric, calcProportion)

data Source = From | To

type MainState = { from :: Metric, to :: Metric, multiplier :: Metric }

data MainAction
  = ChangeMetric Source Metric
  | ChangeIngredient Source PT.RecipePart R.RecipeOutput

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI myApp unit body

myApp :: forall query input output m. MonadEffect m => H.Component query input output m
myApp = H.mkComponent
  { initialState: const newState
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  , render: render
  }
  where
  render state = HH.div [ cls [ "container" ] ]
    [ HH.slot Pie._pie "from" (Pie.pieComponent PT.Regular) unit $ fromPieOutput From
    , HH.div_
        [ HH.text $ "From recipe" <> show state.from
        , HH.text $ "Mult" <> show state.multiplier
        , HH.text $ "To recipe" <> show state.to
        ]
    , HH.slot _frozenPie "to" frozenPieComponent state.multiplier $ fromPieOutput To
    ]

  handleAction = case _ of
    ChangeMetric source metric -> H.modify_ \state -> case source of
      From -> { to: state.to, from: metric, multiplier: calcProportion metric state.to }
      To -> { to: metric, from: state.from, multiplier: calcProportion state.from metric }
    ChangeIngredient source part ingredientAction -> case source of
      From -> case ingredientAction of
        R.IngredientUpdated ing ->
          H.tell _frozenPie "to" $ PT.AddIngredient part ing
        R.IngredientRemoved name ->
          H.tell _frozenPie "to" $ PT.RemoveIngredient part name
      To -> pure unit

newState :: MainState
newState =
  { from: { surface: 0.0, volume: 0.0 }
  , to: { surface: 0.0, volume: 0.0 }
  , multiplier: { surface: 1.0, volume: 1.0 }
  }

fromPieOutput :: Source -> PT.PieOutput -> MainAction
fromPieOutput source pieOutput = case pieOutput of
  (PT.ChangeMoldMetric metric) -> ChangeMetric source metric
  (PT.ChangeIngredient rP rO) -> ChangeIngredient source rP rO
