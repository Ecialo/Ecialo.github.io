module Main exposing (Model, main, view)

-- import Html exposing (Attribute, Html, div, input, text)

import BakeForm exposing (BakeForm, newRectBakeForm, newRoundBakeForm)
import Browser
import Dict exposing (Dict)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Styled exposing (..)
import Message exposing (Message(..))
import Recipe exposing (..)
import Recipe.Types exposing (..)
import Recipe.Views exposing (viewDynRecipe)


type alias Model =
    { from : DynRecipe
    , to : DynRecipe
    }


initialModel : Model
initialModel =
    { from = newDynRecipe
    , to = newDynRecipe
    }


view : Model -> Html Message
view model =
    div [] [ viewDynRecipe model.from "from", viewDynRecipe model.to "to" ]


updateRecipe : List String -> (DynRecipe -> DynRecipe) -> Model -> Model
updateRecipe recipeId mut model =
    case recipeId of
        "from" :: _ ->
            { model | from = mut model.from }

        "to" :: _ ->
            { model | to = mut model.to }

        _ ->
            model


update : Message -> Model -> Model
update msg model =
    case msg of
        AddIngridient recipePath ->
            updateRecipe recipePath addOngoingIngridient model

        ChangeIngridientName recipePath name ->
            updateRecipe recipePath (changeOngoingIngridientName name) model

        ChangeIngridientAmount recipePath amount ->
            updateRecipe recipePath (changeOngoingIngridientAmount amount) model

        ChangeRecipeType recipePath _ ->
            model


main =
    Browser.sandbox
        { init = initialModel
        , view = view >> toUnstyled
        , update = update
        }
