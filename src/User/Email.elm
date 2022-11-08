module User.Email exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)

type Email = Email String

toString : Email -> String
toString (Email str) = str

encode : Email -> Value
encode (Email email_) = Encode.string email_

decoder : Decoder Email
decoder = Decode.map Email Decode.string

