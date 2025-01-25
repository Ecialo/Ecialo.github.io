module Lib.Mold where

import Data.Maybe
import Data.Number
import Data.Traversable
import Data.Tuple
import Prelude
import Type.Proxy

import Control.Alternative (guard)
import Data.Array ((!!))
import Effect.Class (class MonadEffect)
import Effect.Console (logShow)
import Halogen (getRef, lift, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Lib.Types (Metric)
import Web.HTML.HTMLInputElement (select)

_mold = Proxy :: Proxy "mold"

data Dimension
  = Radius
  | Height
  | Width
  | Length

instance dimensionShow :: Show Dimension where
  show = case _ of
    Radius -> "Radius"
    Height -> "Height"
    Width -> "Width"
    Length -> "Length"

data MoldForm
  = RoundMold Number Number
  | RectMold Number Number Number

type Mold =
  { form :: MoldForm
  , isClosed :: Boolean
  }

data MoldAction
  = ChangeForm Int
  | ChangeSize Dimension String
  | Init

instance moldActionShow :: Show MoldAction where
  show = case _ of
    ChangeForm i -> "ChangeForm " <> show i
    ChangeSize d n -> "ChangeSize " <> show d <> " " <> n
    Init -> "Init"

type MoldOutput = Metric

calcVolume :: Mold -> Number
calcVolume mold = case mold.form of
  RoundMold r h -> pi * r * r * h
  RectMold w l h -> w * l * h

calcSurface :: Mold -> Number
calcSurface mold = surface
  where
  closedSurface = case mold.form of
    RoundMold r h -> 2.0 * pi * r * (r + h)
    RectMold w l h -> 2.0 * (w * l + w * h + l * h)
  cap = case mold.form of
    RoundMold r _ -> pi * r * r
    RectMold w l _ -> w * l
  surface = if mold.isClosed then closedSurface else closedSurface - cap

moldComponent :: forall query input m. MonadEffect m => H.Component query input MoldOutput m
moldComponent = H.mkComponent
  { initialState: const simpleMold
  , render: render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = pure Init
      }
  }
  where
  render mold =
    let
      dimensions = case mold.form of
        RoundMold r h -> HH.div_
          [ HH.input [ HP.type_ InputNumber, HP.value $ show r, HE.onValueInput \v -> ChangeSize Radius v ]
          , HH.input [ HP.type_ InputNumber, HP.value $ show h, HE.onValueInput \v -> ChangeSize Height v ]
          ]
        RectMold w l h -> HH.div_
          [ HH.input [ HP.type_ InputNumber, HP.value $ show w, HE.onValueInput \v -> ChangeSize Width v ]
          , HH.input [ HP.type_ InputNumber, HP.value $ show l, HE.onValueInput \v -> ChangeSize Length v ]
          , HH.input [ HP.type_ InputNumber, HP.value $ show h, HE.onValueInput \v -> ChangeSize Height v ]
          ]
      selector = HH.select [ HE.onSelectedIndexChange \i -> ChangeForm i ]
        [ HH.option_ [ HH.text "Round" ]
        , HH.option_ [ HH.text "Rectangular" ]
        ]
    in
      HH.div_
        [ HH.h1_ [ HH.text "Mold" ]
        , selector
        , dimensions
        ]

computeNewmold :: Mold -> MoldAction -> Maybe Mold
computeNewmold state = case _ of
  Init -> Just simpleMold
  ChangeForm i -> do
    case i of
      0 -> Just $ simpleMold { form = RoundMold 10.0 20.0 }
      1 -> Just $ simpleMold { form = RectMold 10.0 20.0 30.0 }
      _ -> Nothing
  ChangeSize d n -> do
    let
      mVal = fromString n
      val = fromMaybe 0.0 mVal
    case d of
      Radius -> case state.form of
        RoundMold _ h -> Just $ state { form = RoundMold val h }
        _ -> Nothing
      Height -> case state.form of
        RoundMold r _ -> Just $ state { form = RoundMold r val }
        RectMold w l _ -> Just $ state { form = RectMold w l val }
      Width -> case state.form of
        RectMold _ l h -> Just $ state { form = RectMold val l h }
        _ -> Nothing
      Length -> case state.form of
        RectMold w _ h -> Just $ state { form = RectMold w val h }
        _ -> Nothing

handleAction :: forall s m. MonadEffect m => MoldAction -> H.HalogenM Mold MoldAction s MoldOutput m Unit
handleAction action = do
  state <- H.get
  let mNewMold = computeNewmold state action
  case mNewMold of
    Just newMold -> do
      let metric = { surface: calcSurface newMold, volume: calcVolume newMold }
      H.put newMold
      H.raise $ metric
    Nothing -> pure unit

simpleMold :: Mold
simpleMold = { form: RoundMold 10.0 20.0, isClosed: true }