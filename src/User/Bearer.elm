module User.Bearer exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type Bearer = Bearer String

toString : Bearer -> String
toString (Bearer str) = str

encode : Bearer -> Value
encode (Bearer bearer_) = Encode.string bearer_

decoder : Decoder Bearer
decoder =
    Decode.map Bearer Decode.string

