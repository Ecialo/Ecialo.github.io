module Recipe exposing (..)

import Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , input
        , li
        , text
        , ul
        )
import Html.Attributes exposing (name)
import Round
import String exposing (fromFloat, fromInt, join)


type alias Recipe =
    List Ingridient


type Ingridient
    = Ingridient String Float


viewRecipe recipe =
    ul [] <| List.map viewIngrient recipe


viewRecipeDyn recipe recipe_id =
    div []
        [ viewRecipe recipe
        , button
        ]


viewIngrient (Ingridient name amount) =
    text <| join " " [ Round.round 2 amount, name ]
