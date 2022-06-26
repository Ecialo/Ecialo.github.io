module Recipe.Views exposing (..)

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( css
        , placeholder
        )
import Html.Styled.Events exposing (onClick, onInput)
import Message exposing (Message(..))
import Recipe.Types exposing (..)
import Round
import String exposing (join)


columnCss =
    css
        [ float left
        , padding2 (px 0) (px 10)
        ]


viewDynRecipe : DynRecipe -> String -> Html Message
viewDynRecipe dynRecipe recipeId =
    case dynRecipe.recipe of
        (Recipe _ _) as recipe ->
            div [ columnCss ]
                [ viewRecipe recipe
                , newIngridientForm [ recipeId ]
                ]

        PastryRecipe _ _ ->
            div [ columnCss ] []

        _ ->
            div [] []


viewRecipe : Recipe -> Html Message
viewRecipe recipe =
    case recipe of
        Recipe _ r ->
            ul [] <| List.map viewIngridient r

        _ ->
            div [] []


newIngridientForm recipeId =
    div []
        [ input [ placeholder "Сколько", onInput <| ChangeIngridientAmount recipeId ] []
        , input [ placeholder "Чего", onInput <| ChangeIngridientName recipeId ] []
        , button [ onClick <| AddIngridient recipeId ] [ text "+" ]
        ]


viewIngridient (Ingridient name amount) =
    li [] [ text <| join " " [ Round.round 2 amount, name ] ]


viewBakeForm bakeForm =
    div [] []
