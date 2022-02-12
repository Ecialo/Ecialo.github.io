module Message exposing (..)


type Message
    = AddIngridient { recipe_id : String, name : String, amount : Float }
