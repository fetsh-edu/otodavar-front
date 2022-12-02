port module SharedModel exposing (..)

import Browser.Navigation as Nav
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Notifications exposing (Notification, Notifications)
import OtoApi exposing (config)
import Push exposing (Push)
import RemoteData exposing (WebData)
import RemoteData.Http
import Url exposing (Url)
import User.Bearer as Bearer exposing (Bearer)
import User.User as User exposing (SimpleInfo, User)

type alias SharedModel =
    { key : Nav.Key
    , url : Url
    , drawer : Bool
    , currentUrl : Url
    , apiUrl : Url
    , auth : Auth
    }

type Auth
    = LoggedIn User Notifications Push.Model
    | Guest (Maybe Error)

type Error
    = ErrCacheParse Decode.Error
    | ErrInfoGet Http.Error

guest : Nav.Key -> Url -> Url-> Url -> SharedModel
guest key url_ currentUrl apiUrl = SharedModel key url_ False currentUrl apiUrl (Guest Nothing)


bearer : SharedModel -> Maybe Bearer
bearer = Maybe.map User.bearer << user

navKey : SharedModel -> Nav.Key
navKey = .key

url : SharedModel -> Url
url = .url

notifications : SharedModel -> Maybe Notifications
notifications session =
    case session.auth of
        LoggedIn _ n _ -> Just n
        Guest _ -> Nothing

push : SharedModel -> Maybe Push.Model
push sharedModel =
    case sharedModel.auth of
        LoggedIn _ _ p_ -> Just p_
        Guest _ -> Nothing

setNotifications : WebData (List Notification) -> SharedModel -> SharedModel
setNotifications n_ s_ =
    case s_.auth of
        Guest _ -> s_
        LoggedIn user_ ns_ p_ -> {s_ | auth = LoggedIn user_ { ns_ | items = n_} p_ }

setPush : Push.Model -> SharedModel -> SharedModel
setPush p_ s_ =
    case s_.auth of
        Guest _ -> s_
        LoggedIn u_ ns_ oldP_ -> { s_ | auth = LoggedIn u_ ns_ p_ }

disablePushButton : SharedModel -> SharedModel
disablePushButton s_ =
    case s_.auth of
        Guest _ -> s_
        LoggedIn u_ ns_ oldP_ -> { s_ | auth = LoggedIn u_ ns_ { oldP_ | buttonDisabled = True } }

enablePushButton : SharedModel -> SharedModel
enablePushButton s_ =
    case s_.auth of
        Guest _ -> s_
        LoggedIn u_ ns_ oldP_ -> { s_ | auth = LoggedIn u_ ns_ { oldP_ | buttonDisabled = False } }


updateNotifications : (Notifications -> Notifications)  -> SharedModel -> SharedModel
updateNotifications f s_ =
    case s_.auth of
        Guest _ -> s_
        LoggedIn user_ ns_ p_ -> {s_ | auth = LoggedIn user_ (f ns_) p_ }



updateUserInfo : WebData SimpleInfo -> SharedModel -> SharedModel
updateUserInfo userInfo oldModel =
    case oldModel.auth of
        Guest _ -> oldModel
        LoggedIn user_ ns_ p_ ->
            case userInfo of
                RemoteData.Failure e -> { oldModel | auth = Guest (Just <| ErrInfoGet e) }
                RemoteData.Success a -> { oldModel | auth = LoggedIn (User.updateInfo a user_) ns_ p_ }
                _ -> oldModel


isGuest : SharedModel -> Bool
isGuest sm =
    case sm.auth of
        LoggedIn _ _ _ -> False
        _ -> True

user : SharedModel -> Maybe User
user sm =
    case sm.auth of
        LoggedIn val _ _ ->
            Just val
        Guest _ ->
            Nothing

login : User -> Cmd msg
login user_ =
    storeSession (Just (User.encode user_))

logout : Cmd msg
logout =
    storeSession Nothing

changes : (SharedModel -> msg) -> SharedModel -> Sub msg
changes toMsg session =
    onSessionChange (\val -> toMsg (decode session val))

decode : SharedModel -> Decode.Value -> SharedModel
decode oldModel value =
    let
        newAuth =
            case
                Decode.decodeValue User.decoderNullable value
            of
                Ok (Just decodedViewer) ->
                    LoggedIn
                        decodedViewer
                        (notifications oldModel |> Maybe.withDefault Notifications.initModel)
                        (push oldModel |> Maybe.withDefault Push.init)

                Ok Nothing ->
                    Guest Nothing
                Err error ->
                    Guest (Just (ErrCacheParse error))
    in
    { oldModel | auth = newAuth}

port storeSession : Maybe Decode.Value -> Cmd msg
port onSessionChange : (Encode.Value -> msg) -> Sub msg