module User.Avatar exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type Avatar = Avatar String

toString : Avatar -> String
toString (Avatar str) = str

encode : Avatar -> Value
encode (Avatar avatar_) = Encode.string avatar_

decoder : Decoder Avatar
decoder =
    Decode.map Avatar Decode.string