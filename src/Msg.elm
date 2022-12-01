module Msg exposing (..)

import Browser
import Game
import Home
import Login
import Notifications
import Profile
import Push
import RemoteData exposing (WebData)
import SharedModel exposing (SharedModel)
import Url exposing (Url)
import User.Uid exposing (Uid)
import User.User exposing (SimpleInfo)

type Msg
    = NoOp
    | ToggleDarkMode
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | AuthEmerged SharedModel
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotProfileMsg Profile.Msg
    | GotGameMsg Game.Msg
    | GotNotificationsMsg Notifications.Msg
    | GotPushMsg Push.Msg
    | UserInfoReceived (WebData SimpleInfo)
    | HideNotifications
    | ShowNotifications
    | HideDrawer
    | ShowDrawer
    | LaunchGame (Maybe Uid)