module User.Avatar exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (attribute, class, src, style)
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

img : Avatar -> String -> Html msg
img avatar classes =
    Html.img
        [ src (toString avatar)
        , attribute "referrerpolicy" "no-referrer"
        , class "rounded-lg"
        , class classes
        , style "min-width" "48px"
        ] []