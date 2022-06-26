module Recipe exposing (..)

import Recipe.Types exposing (..)


changeOngoingIngridientAmount : String -> DynRecipe -> DynRecipe
changeOngoingIngridientAmount amount recipe =
    let
        newIngrid =
            case recipe.ongoingIngridient of
                Nothing ->
                    parseIngridient "Something" amount

                Just (Ingridient name _) ->
                    parseIngridient name amount
    in
    { recipe | ongoingIngridient = newIngrid }


changeOngoingIngridientName : String -> DynRecipe -> DynRecipe
changeOngoingIngridientName name recipe =
    let
        newIngrid =
            case recipe.ongoingIngridient of
                Nothing ->
                    Just <| Ingridient name 0

                Just (Ingridient _ amount) ->
                    Just <| Ingridient name amount
    in
    { recipe | ongoingIngridient = newIngrid }


addIngridient : Ingridient -> Recipe -> Recipe
addIngridient ingridient recipe =
    case recipe of
        Recipe rt ingridients ->
            Recipe rt (ingridients ++ [ ingridient ])

        _ ->
            recipe


addOngoingIngridient : DynRecipe -> DynRecipe
addOngoingIngridient dynRecipe =
    case dynRecipe.ongoingIngridient of
        Nothing ->
            dynRecipe

        Just ingridient ->
            let
                new_recipe =
                    addIngridient ingridient dynRecipe.recipe
            in
            { dynRecipe | recipe = new_recipe, ongoingIngridient = Nothing }
