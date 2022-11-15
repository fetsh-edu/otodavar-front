port module Session exposing (..)

import Browser.Navigation as Nav
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Notifications exposing (Notification, Notifications)
import OAuth.Implicit as OAuth exposing (AuthorizationResultWith(..))
import RemoteData exposing (WebData)
import Url exposing (Url)
import User.Bearer exposing (Bearer)
import User.User as User exposing (User)

type Session
    = LoggedIn Nav.Key Url Notifications User
    | Guest Nav.Key Url (Maybe Error)

type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo Http.Error
    | ErrCacheParse Decode.Error

guest : Nav.Key -> Url -> Session
guest key url_ = Guest key url_ Nothing


bearer : Session -> Maybe Bearer
bearer = Maybe.map User.bearer << user

navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ _ _ ->
            key

        Guest key _ _ ->
            key

url : Session -> Url
url session =
    case session of
        LoggedIn _ url_ _ _ -> url_
        Guest _ url_ _ -> url_

notifications : Session -> Maybe Notifications
notifications session =
    case session of
        LoggedIn _ _ n _ -> Just n
        Guest _ _ _ -> Nothing

setNotifications : WebData (List Notification)  -> Session -> Session
setNotifications n_ s_ =
    case s_ of
        LoggedIn k_ url_ ns_ user_ -> LoggedIn k_ url_ {ns_ | items = n_} user_
        Guest _ _ _ -> s_

updateNotifications : (Notifications -> Notifications)  -> Session -> Session
updateNotifications f s_ =
    case s_ of
        LoggedIn k_ url_ ns_ user_ -> LoggedIn k_ url_ (f ns_) user_
        Guest _ _ _ -> s_


isGuest : Session -> Bool
isGuest session =
    case session of
        LoggedIn _ _ _ _ -> False
        _ -> True

user : Session -> Maybe User
user session =
    case session of
        LoggedIn _ _ _ val ->
            Just val

        Guest _ _ _ ->
            Nothing

login : User -> Cmd msg
login user_ =
    storeSession (Just (User.encode user_))

logout : Cmd msg
logout =
    storeSession Nothing

changes : (Session -> msg) -> Session -> Sub msg
changes toMsg session =
    onSessionChange (\val -> toMsg (decode session val))
    --onSessionChange (\val -> toMsg (Decode.decodeValue User.decoderNullable))

decode : Session -> Decode.Value -> Session
decode session value =
    case
        Decode.decodeValue User.decoderNullable value
    of
        Ok (Just decodedViewer) ->
            LoggedIn
                (navKey session)
                (url session)
                (notifications session |> Maybe.withDefault Notifications.initModel)
                decodedViewer
        Ok Nothing ->
            Guest (navKey session) (url session) Nothing
        Err error ->
            Guest (navKey session) (url session) (Just (ErrCacheParse (Debug.log "Error: " error)))

port storeSession : Maybe Decode.Value -> Cmd msg
port onSessionChange : (Encode.Value -> msg) -> Sub msg