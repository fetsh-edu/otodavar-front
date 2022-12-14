module Game.Word exposing (..)

import Game.Stamp as Stamp exposing (Stamp)
import Html exposing (Attribute)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import User.Uid as Uid exposing (Uid)

shrinkThreshold : Int
shrinkThreshold = 13

type alias Word =
    { word: String
    , player: Uid
    , roundId : Int
    , id : Int
    , stamp : Stamp
    }

decoder : Decoder Word
decoder =
    Decode.map5 Word
        (Decode.field "word" Decode.string)
        (Decode.field "player" Uid.decoder)
        (Decode.field "round_id" Decode.int)
        (Decode.field "id" Decode.int)
        (Decode.field "stamp" Stamp.decoder)

encoder : Uid -> Int -> String -> Value
encoder uid roundId guess =
    Encode.object
        [ ( "game_uid", Encode.string (Uid.toString uid) )
        , ( "word", Encode.object [ ("round_id", Encode.int roundId), ("word", Encode.string guess) ] )
        ]

-- VIEW

shrinkClass : String -> Attribute msg
shrinkClass word =
    if String.length word > shrinkThreshold
        then Html.Attributes.class "text-sm"
        else Html.Attributes.class ""