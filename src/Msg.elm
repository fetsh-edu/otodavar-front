module Msg exposing (..)

import Browser
import Game
import Home
import Login
import Notifications
import Profile
import Session exposing (Session)
import Url exposing (Url)
import User.Uid exposing (Uid)

type Msg
    = NoOp
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | SessionEmerged Session
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotProfileMsg Profile.Msg
    | GotGameMsg Game.Msg
    | GotNotificationsMsg Notifications.Msg
    | HideNotifications
    | ShowNotifications
    | LaunchGame (Maybe Uid)