module Msg exposing (..)

import Browser
import Game
import Home
import Login
import Notifications
import Profile
import RemoteData exposing (WebData)
import SharedModel exposing (SharedModel)
import Url exposing (Url)
import User.Uid exposing (Uid)
import User.User exposing (SimpleInfo)

type Msg
    = NoOp
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | AuthEmerged SharedModel
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotProfileMsg Profile.Msg
    | GotGameMsg Game.Msg
    | GotNotificationsMsg Notifications.Msg
    | UserInfoReceived (WebData SimpleInfo)
    | HideNotifications
    | ShowNotifications
    | LaunchGame (Maybe Uid)