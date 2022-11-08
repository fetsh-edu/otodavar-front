module User.Name exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type Name = Name String

toString : Name -> String
toString (Name str) = str

encode : Name -> Value
encode (Name name_) = Encode.string name_

decoder : Decoder Name
decoder =Decode.map Name Decode.string