module Msg exposing (..)

import Browser
import Home
import Login
import Notifications
import Profile
import Session exposing (Session)
import Url exposing (Url)

type Msg
    = NoOp
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | SessionEmerged Session
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotProfileMsg Profile.Msg
    | GotNotificationsMsg Notifications.Msg
    | HideNotifications
    | ShowNotifications