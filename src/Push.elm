port module Push exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode

type Push
    = NotAsked
    | NotSupported
    | Error String
    | Denied
    | Subscribed
    | Unsubscribed

type Msg
    = GotPushChange Push
    | Subscribe
    | UnSubscribe

decoder : Decode.Decoder Push
decoder = Decode.string |> Decode.andThen (fromString >> Decode.succeed)

encode : Push -> Encode.Value
encode = toString >> Encode.string

toMsg : Push -> Cmd Msg
toMsg push =
    case push of
        NotAsked -> Cmd.none
        NotSupported -> Cmd.none
        Subscribed -> Debug.log "TODO: Subscribed command message" Cmd.none
        Unsubscribed -> Debug.log "TODO: Unsubscribe command message" Cmd.none
        Error string -> Debug.log "TODO: Unsubscribe command message" Cmd.none
        Denied -> Debug.log "TODO: Unsubscribe command message" Cmd.none



toString : Push -> String
toString status =
    case status of
        NotSupported -> "not_supported"
        Error string -> "error"
        Denied -> "denied"
        Subscribed -> "subscribed"
        Unsubscribed -> "unsubscribed"
        NotAsked -> "not_asked"


fromString : String -> Push
fromString str =
    case str of
        "not_supported" -> NotSupported
        "denied" -> Denied
        "subscribed" -> Subscribed
        "unsubscribed" -> Unsubscribed
        "error" -> Error "error"
        _ -> Error str

resultToPush : Result Decode.Error Push -> Push
resultToPush a =
    case a of
        Ok value -> value
        Err error -> Error (Decode.errorToString error)


onPushChangeDecoded : (Push -> msg) ->Sub msg
onPushChangeDecoded toMsg_ =
    onPushChange (Decode.decodeValue decoder >> resultToPush >> toMsg_)

port onPushChange : (Encode.Value -> msg) -> Sub msg

port subscribePush : () -> Cmd msg
port unsubscribePush : () -> Cmd msg