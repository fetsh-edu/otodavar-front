module User.Uid exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type Uid = Uid String

toString : Uid -> String
toString (Uid uid) = uid

encode : Uid -> Value
encode (Uid uid) = Encode.string uid

decoder : Decoder Uid
decoder =
    Decode.map Uid Decode.string
