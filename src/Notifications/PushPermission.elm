port module Notifications.PushPermission exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode

port receivedPermission : (Encode.Value -> msg) -> Sub msg
port requestPermission : () -> Cmd msg


type Permission
    = Granted
    | Denied
    | Default
    | NotAsked
    | NotSupported
    | Error String

toString : Permission -> String
toString perm =
    case perm of
        Granted -> "granted"
        Denied -> "denied"
        Default -> "default"
        NotAsked -> "not_asked"
        NotSupported -> "not_supported"
        Error _ -> "error"



fromString : String -> Permission
fromString str =
    case str of
        "granted" -> Granted
        "denied" -> Denied
        "default" -> Default
        "not_supported" -> NotSupported
        other -> Error other

decoder : Decode.Decoder Permission
decoder =
    Decode.string |> Decode.andThen (fromString >> Decode.succeed)