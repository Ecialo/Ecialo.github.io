module BakeForm exposing (..)

import Html.Attributes exposing (shape)
import Measure exposing (..)


type BakeForm
    = BakeForm { shape : Shape, isClosed : Bool }


type Shape
    = Round { d : Float, h : Float }
    | Rect { x : Float, y : Float, h : Float }


newRoundBakeForm ( d, h ) isClosed =
    BakeForm { shape = Round { d = d, h = h }, isClosed = isClosed }


newRectBakeForm ( x, y, h ) isClosed =
    BakeForm { shape = Rect { x = x, y = y, h = h }, isClosed = isClosed }


computeV : BakeForm -> V
computeV (BakeForm { shape }) =
    case shape of
        Round { d, h } ->
            h * circS d

        Rect { x, y, h } ->
            x * y * h


computeS : BakeForm -> S
computeS (BakeForm { shape, isClosed }) =
    let
        bases =
            if isClosed then
                2

            else
                1
    in
    case shape of
        Round { d, h } ->
            circumference d * h + bases * circS d

        Rect { x, y, h } ->
            2 * (x * h + y * h) + bases * x * y


circS : Float -> S
circS d =
    pi * d / 4


circumference : Float -> Float
circumference d =
    pi * d
