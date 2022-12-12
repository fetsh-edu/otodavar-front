module Msg exposing (..)

import Browser
import Game
import Game.ExampleGame as GameExample
import Home
import Login
import Notifications
import Notifications.BrowserNotifications as BrowserNotifications
import Profile
import ProfileEdit
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
    | GotProfileEditMsg ProfileEdit.Msg
    | GotGameMsg Game.Msg
    | GotExampleGameMsg GameExample.Msg
    | GotNotificationsMsg Notifications.Msg
    | GotBrowserNotificationsMsg BrowserNotifications.Msg
    | UserInfoReceived (WebData SimpleInfo)
    | HideNotifications
    | ShowNotifications
    | HideDrawer
    | ShowDrawer
    | AlertDismiss
    | OnGameArchived
    | LaunchGame (Maybe Uid)