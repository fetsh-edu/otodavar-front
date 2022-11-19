module User.FriendStatus exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)

type Status
    = Me
    | Friend
    | Requested
    | Wannabe
    | Unknown

decoder : Decode.Decoder Status
decoder =
    Decode.string |> Decode.andThen (fromString >> Decode.succeed)

encode : Status -> Value
encode = toString >> Encode.string


toString : Status -> String
toString status =
    case status of
        Me -> "me"
        Friend -> "friend"
        Requested -> "requested"
        Wannabe -> "wannabe"
        Unknown -> "unknown"

fromString : String -> Status
fromString str =
    case str of
        "me" -> Me
        "friend" -> Friend
        "requested" -> Requested
        "wannabe" -> Wannabe
        _ -> Unknown