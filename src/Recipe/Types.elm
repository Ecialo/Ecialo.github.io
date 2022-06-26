module Recipe.Types exposing (..)

import BakeForm exposing (..)



-- import


type RecipeType
    = Whole
    | Shell
    | Staff


type Recipe
    = Recipe RecipeType (List Ingridient)
    | MultiRecipe (List ( String, Recipe ))
    | PastryRecipe BakeForm Recipe


type alias DynRecipe =
    { recipe : Recipe
    , ongoingIngridient : Maybe Ingridient
    }


type Ingridient
    = Ingridient String Float


parseIngridient : String -> String -> Maybe Ingridient
parseIngridient name amount =
    amount
        |> String.toFloat
        |> Maybe.map (\parsedValue -> Ingridient name parsedValue)


newDynRecipe : DynRecipe
newDynRecipe =
    { recipe = newRecipe
    , ongoingIngridient = Nothing
    }


newRecipe : Recipe
newRecipe =
    Recipe Whole []



-- newPastryRecipe =
-- PastryRecipe (newRoundBakeForm ( 0, 0 ) True) <| MultiRecipe [("Тесто", newRecipe), ("Начинка")]
