port module Session exposing (..)

import Browser.Navigation as Nav
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import OAuth.Implicit as OAuth exposing (AuthorizationResultWith(..))
import Url exposing (Url)
import User.User as User exposing (User)

type Session
    = LoggedIn Nav.Key Url User
    | Guest Nav.Key Url (Maybe Error)

type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo Http.Error
    | ErrCacheParse Decode.Error


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ _ ->
            key

        Guest key _ _ ->
            key

url : Session -> Url
url session =
    case session of
        LoggedIn _ url_ _ -> url_
        Guest _ url_ _ -> url_


isGuest : Session -> Bool
isGuest session =
    case session of
        LoggedIn _ _ _ -> False
        _ -> True

user : Session -> Maybe User
user session =
    case session of
        LoggedIn _ _ val ->
            Just val

        Guest _ _ _ ->
            Nothing

login : User -> Cmd msg
login user_ =
    storeSession (Just (User.encode user_))

logout : Cmd msg
logout =
    storeSession Nothing

changes : (Session -> msg) -> Nav.Key -> Url -> Sub msg
changes toMsg key url_ =
    onSessionChange (\val -> toMsg (decode key url_ val))
    --onSessionChange (\val -> toMsg (Decode.decodeValue User.decoderNullable))

decode : Nav.Key -> Url -> Decode.Value -> Session
decode key url_ value =
    case
        Decode.decodeValue User.decoderNullable value
    of
        Ok (Just decodedViewer) ->
            LoggedIn key url_ decodedViewer
        Ok Nothing ->
            Guest key url_ Nothing
        Err error ->
            Guest key url_ (Just (ErrCacheParse error))

port storeSession : Maybe Decode.Value -> Cmd msg
port onSessionChange : (Encode.Value -> msg) -> Sub msg