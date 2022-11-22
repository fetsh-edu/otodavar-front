module Game.Word exposing (..)

import Json.Decode as Decode exposing (Decoder)
import User.Uid as Uid exposing (Uid)

type alias Word =
    { word: String
    , player: Uid
    , roundId : Int
    }

decoder : Decoder Word
decoder =
    Decode.map3 Word
        (Decode.field "word" Decode.string)
        (Decode.field "player" Uid.decoder)
        (Decode.field "round_id" Decode.int)