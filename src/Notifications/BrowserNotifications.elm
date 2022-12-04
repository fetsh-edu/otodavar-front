module Notifications.BrowserNotifications exposing (..)

import Json.Decode as Decode
import Notifications.PushPermission as Permission exposing (Permission)

type alias BrowserNotifications =
    { permission : Result Decode.Error Permission
    }

init : BrowserNotifications
init =
    { permission = Ok Permission.NotAsked
    }

type Msg
    = GotPermission (Result Decode.Error Permission)


update : Msg -> BrowserNotifications -> ( BrowserNotifications, Cmd Msg )
update msg model =
    case msg of
        GotPermission permission ->
           ( { model | permission = permission }, Cmd.none)

--type Msg
    --=
    --= GotPushChange Push
    --| GotPermissionChange (Result Error Permission)
    --| Subscribe
    --| UnSubscribe
    --| HandlePushResponse (WebData String)