port module Push exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode

port subscribePush : () -> Cmd msg
port unsubscribePush : () -> Cmd msg
port onPushChange : (Encode.Value -> msg) -> Sub msg

type Push
    = NotAsked
    | Error String
    | Subscribed String
    | Unsubscribed (Maybe String)
--
--
--
--
----update : Msg -> model -> ( model, Cmd Msg )
--update : Msg -> model -> (model -> SharedModel.SharedModel) -> (SharedModel.SharedModel -> model -> model) -> (model, Cmd Msg)
--update msg model getSharedModel updateSharedModel =
--    case (msg, model) of
--        ((GotPushChange push), _) ->
--                let
--                    sharedModel = model |> getSharedModel
--                    newModel = model |> updateSharedModel (sharedModel |> SharedModel.setPush { state = push, buttonDisabled = False } )
--                    newCommand = push |> toCmd (sharedModel.apiUrl) (SharedModel.bearer sharedModel)
--                in
--                ( newModel
--                , newCommand
--                )
--        (Subscribe, _) ->
--                ( model |> updateSharedModel (model |> getSharedModel |> SharedModel.disablePushButton )
--                , subscribePush ()
--                )
--        (UnSubscribe, _) ->
--                ( model |> updateSharedModel (model |> getSharedModel |> SharedModel.enablePushButton )
--                , unsubscribePush ()
--                )
--        ((HandlePushResponse resp), _) ->
--                case resp of
--                    RemoteData.Failure e ->
--                        ( model |> updateSharedModel (model |> getSharedModel |> SharedModel.setPush { state = Error "server", buttonDisabled = False } )
--                        , Cmd.none
--                        )
--
--                    _ -> (model, Cmd.none)
--
--
--
decoder : Decode.Decoder Push
decoder =
    Decode.field "status" Decode.string
        |> Decode.andThen (\str ->
            case str of
                "unsubscribed" -> Decode.map Unsubscribed (Decode.field "payload" (Decode.nullable Decode.string))
                "subscribed" -> Decode.map Subscribed (Decode.field "payload" Decode.string)
                "error" -> Decode.map Error (Decode.field "error" Decode.string)
                _ -> Decode.succeed <| Error str
        )
--
--
--save : Url -> Maybe Bearer -> Push -> Cmd Msg
--save apiUrl maybeBearer push =
--    let
--        url = (OtoApi.routes apiUrl).push
--        message bearer =
--            case push of
--                NotAsked -> Cmd.none
--                NotSupported -> Cmd.none
--                Denied -> Cmd.none
--                Unsubscribed maybeString ->
--                    RemoteData.Http.deleteWithConfig (OtoApi.config bearer) url HandlePushResponse (encode push)
--                Subscribed string ->
--                    RemoteData.Http.postWithConfig (OtoApi.config bearer) url HandlePushResponse (Decode.string) (encode push)
--                Error string ->
--                    RemoteData.Http.postWithConfig (OtoApi.config bearer) url HandlePushResponse (Decode.string) (encode push)
--
--    in
--    maybeBearer |> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none
--
--
encode : Push -> Encode.Value
encode push =
    case push of
        Error _ -> unSubValue Nothing
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

fromResult : Result Decode.Error Push -> Push
fromResult a =
    case a of
        Ok value -> value
        Err error -> Error (Decode.errorToString error)

