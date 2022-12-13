module User.Handle exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type Handle = Handle String

toString : Handle -> String
toString (Handle str) = str

encode : Handle -> Value
encode (Handle str) = Encode.string str

decoder : Decoder Handle
decoder =
    Decode.map Handle Decode.string
