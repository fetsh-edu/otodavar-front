port module Push exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import OtoApi
import RemoteData exposing (WebData)
import RemoteData.Http
import Url exposing (Url)
import User.Bearer as Bearer exposing (Bearer)

type alias Model =
    { buttonDisabled : Bool
    , state: Push
    }

init : Model
init = Model False NotAsked

type Push
    = NotAsked
    | NotSupported
    | Error String
    | Denied
    | Subscribed String
    | Unsubscribed (Maybe String)

type Msg
    = GotPushChange Push
    | Subscribe
    | UnSubscribe
    | HandlePushResponse (WebData String)


decoder : Decode.Decoder Push
decoder =
    Decode.field "status" Decode.string
        |> Decode.andThen (\str ->
            case str of
                "not_supported" -> Decode.succeed NotSupported
                "denied" -> Decode.succeed Denied
                "unsubscribed" -> Decode.map Unsubscribed (Decode.field "payload" (Decode.nullable Decode.string))
                "subscribed" -> Decode.map Subscribed (Decode.field "payload" Decode.string)
                "error" -> Decode.map Error (Decode.field "error" Decode.string)
                _ -> Decode.succeed <| Error str
        )


save : Url -> Maybe Bearer -> Push -> Cmd Msg
save apiUrl maybeBearer push =
    let
        url = (OtoApi.routes apiUrl).push
        message bearer =
            case push of
                NotAsked -> Cmd.none
                NotSupported -> Cmd.none
                Denied -> Cmd.none
                Unsubscribed maybeString ->
                    RemoteData.Http.deleteWithConfig (OtoApi.config bearer) url HandlePushResponse (encode push)
                Subscribed string ->
                    RemoteData.Http.postWithConfig (OtoApi.config bearer) url HandlePushResponse (Decode.string) (encode push)
                Error string ->
                    RemoteData.Http.postWithConfig (OtoApi.config bearer) url HandlePushResponse (Decode.string) (encode push)

    in
    maybeBearer |> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


encode : Push -> Encode.Value
encode push =
    case push of
        NotSupported -> unSubValue Nothing
        Error string -> unSubValue Nothing
        Denied -> unSubValue Nothing
        Subscribed sub -> subValue sub
        Unsubscribed sub -> unSubValue sub
        NotAsked -> unSubValue Nothing

subValue : String -> Encode.Value
subValue str = Encode.object [ ( "subscription", Encode.string str ) ]

unSubValue : Maybe String -> Encode.Value
unSubValue str =
    case str of
        Nothing -> Encode.object [ ( "subscription", Encode.null ) ]
        Just something -> subValue something


toCmd : Url -> Maybe Bearer -> Push -> Cmd Msg
toCmd url maybeBearer push =
    case push of
        NotAsked -> Cmd.none
        NotSupported -> Cmd.none
        _ -> save url maybeBearer push


fromResult : Result Decode.Error Push -> Push
fromResult a =
    case a of
        Ok value -> value
        Err error -> Error (Decode.errorToString error)


onPushChangeDecoded : (Push -> msg) ->Sub msg
onPushChangeDecoded toMsg_ =
    onPushChange (Decode.decodeValue decoder >> fromResult >> toMsg_)

port onPushChange : (Encode.Value -> msg) -> Sub msg

port subscribePush : () -> Cmd msg
port unsubscribePush : () -> Cmd msg