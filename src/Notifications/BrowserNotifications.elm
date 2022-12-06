module Notifications.BrowserNotifications exposing (..)

import Json.Decode as Decode
import Notifications.PushPermission as Permission exposing (Permission)
import OtoApi
import Push exposing (Push)
import RemoteData exposing (WebData)
import RemoteData.Http
import Url exposing (Url)
import User.Bearer as Bearer exposing (Bearer)

type alias BrowserNotifications =
    { permission : Result Decode.Error Permission
    , push : Push
    }

init : BrowserNotifications
init =
    { permission = Ok Permission.NotAsked
    , push = Push.NotAsked
    }

type Msg
    = GotPermission (Result Decode.Error Permission)
    | GotPushChange Push
    | RequestPermission
    | Subscribe
    | Unsubscribe
    | HandlePushResponse (WebData String)


isSubscribed : BrowserNotifications -> Bool
isSubscribed ns =
    case (ns.push, ns.permission) of
        (Push.Subscribed _,  Ok Permission.Granted) ->
            True
        _ ->
            False

update : {apiUrl : Url, bearer : Maybe Bearer} -> Msg -> BrowserNotifications -> ( BrowserNotifications, Cmd Msg )
update p msg model =
    case msg of
        GotPermission permission ->
           ( { model | permission = permission }, Cmd.none)

        RequestPermission ->
            ( model, Permission.requestPermission () )

        Subscribe ->
            ( model, Push.subscribePush ())

        Unsubscribe ->
            ( model, Push.unsubscribePush ())

        GotPushChange push ->
            ( { model | push = push }, save p.apiUrl p.bearer push)

        HandlePushResponse _ ->
            (model, Cmd.none)


save : Url -> Maybe Bearer -> Push -> Cmd Msg
save apiUrl maybeBearer push =
    let
        url = (OtoApi.routes apiUrl).push
        message bearer =
            case push of
                Push.NotAsked -> Cmd.none
                Push.Error _ -> RemoteData.Http.postWithConfig (OtoApi.config bearer) url HandlePushResponse (Decode.string) (Push.encode push)
                Push.Subscribed _ -> RemoteData.Http.postWithConfig (OtoApi.config bearer) url HandlePushResponse (Decode.string) (Push.encode push)
                Push.Unsubscribed _ -> RemoteData.Http.deleteWithConfig (OtoApi.config bearer) url HandlePushResponse (Push.encode push)
    in
    maybeBearer |> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none
