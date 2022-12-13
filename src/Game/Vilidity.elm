module Game.Vilidity exposing (..)

import Json.Decode as Decode
type Validity
    = Valid
    | Invalid (List String)

decoder : Decode.Decoder Validity
decoder =
    Decode.field "valid" Decode.bool
        |> Decode.andThen (\x ->
            if x
            then Decode.succeed Valid
            else Decode.map Invalid (Decode.field "errors" (Decode.list Decode.string))
        )
