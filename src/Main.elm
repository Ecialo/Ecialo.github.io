module Main exposing (Model, Msg(..), main, view)

import BakeForm exposing (BakeForm, newRectBakeForm, newRoundBakeForm)
import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    { from : BakeForm
    , to : BakeForm
    }


initialModel =
    { from = newRectBakeForm ( 10, 10, 10 ) False
    , to = newRectBakeForm ( 10, 10, 10 ) False
    }


type Msg
    = Increment
    | Decrement


view : Model -> Html Msg
view model =
    div []
        []


update : Msg -> Model -> Model
update msg model =
    model


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
