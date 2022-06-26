module Message exposing (..)


type alias RecipePath =
    List String


type Message
    = AddIngridient RecipePath
    | ChangeIngridientName RecipePath String
    | ChangeIngridientAmount RecipePath String
    | ChangeRecipeType RecipePath String



-- = AddIngridient { recipe_id : String, name : String, amount : Float }
