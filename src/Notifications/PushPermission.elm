port module Notifications.PushPermission exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode

port receivedPermission : (Encode.Value -> msg) -> Sub msg


type Permission
    = Granted
    | Denied
    | Default
    | NotAsked

toString : Permission -> String
toString perm =
    case perm of
        Granted -> "granted"
        Denied -> "denied"
        Default -> "default"
        NotAsked -> "not_asked"


fromString : String -> Result String Permission
fromString str =
    case str of
        "granted" -> Ok Granted
        "denied" -> Ok Denied
        "default" -> Ok Default
        other -> Err other

decoder : Decode.Decoder Permission
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case fromString str of
                    Ok value -> Decode.succeed value
                    Err error -> Decode.fail error
            )