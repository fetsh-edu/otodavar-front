module Game.Page exposing (..)

import Json.Decode as Decode

type alias Page a =
    { page : Int
    , totalPages : Int
    , items : List a
    }


decoder : Decode.Decoder a -> Decode.Decoder (Page a)
decoder itemDecoder =
    Decode.map3 Page
        (Decode.field "page" Decode.int)
        (Decode.field "total_pages" Decode.int)
        (Decode.field "items" (Decode.list itemDecoder))