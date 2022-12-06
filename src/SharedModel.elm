port module SharedModel exposing (..)

import Browser.Navigation as Nav
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Notifications exposing (Notification, Notifications)
import Notifications.BrowserNotifications as BrowserNotifications exposing (BrowserNotifications)
import RemoteData exposing (WebData)
import Url exposing (Url)
import User.Bearer exposing (Bearer)
import User.User as User exposing (SimpleInfo, User)

type alias SharedModel =
    { key : Nav.Key
    , url : Url
    , drawer : Bool
    , currentUrl : Url
    , apiUrl : Url
    , auth : Auth
    , alertDismissed : Bool
    }


type Auth
    = LoggedIn User Notifications BrowserNotifications
    | Guest (Maybe Error)

type Error
    = ErrCacheParse Decode.Error
    | ErrInfoGet Http.Error

guest : Nav.Key -> Url -> Url-> Url -> Bool -> SharedModel
guest key url_ currentUrl apiUrl alertDismissed = SharedModel key url_ False currentUrl apiUrl (Guest Nothing) alertDismissed


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

browserNotifications : SharedModel -> Maybe BrowserNotifications
browserNotifications sharedModel =
    case sharedModel.auth of
        LoggedIn _ _ p_ -> Just p_
        Guest _ -> Nothing


setNotifications : WebData (List Notification) -> SharedModel -> SharedModel
setNotifications n_ s_ =
    case s_.auth of
        Guest _ -> s_
        LoggedIn user_ ns_ p_ -> {s_ | auth = LoggedIn user_ { ns_ | items = n_} p_ }

setBrowserNotifications : BrowserNotifications -> SharedModel -> SharedModel
setBrowserNotifications p_ s_ =
    case s_.auth of
        Guest _ -> s_
        LoggedIn u_ ns_ oldP_ -> { s_ | auth = LoggedIn u_ ns_ p_}


updateNotifications : (Notifications -> Notifications)  -> SharedModel -> SharedModel
updateNotifications f s_ =
    case s_.auth of
        Guest _ -> s_
        LoggedIn user_ ns_ p_ -> {s_ | auth = LoggedIn user_ (f ns_) p_ }



updateUserInfoWD : WebData SimpleInfo -> SharedModel -> SharedModel
updateUserInfoWD userInfo oldModel =
    case oldModel.auth of
        Guest _ -> oldModel
        LoggedIn user_ ns_ p_ ->
            case userInfo of
                RemoteData.Failure e -> { oldModel | auth = Guest (Just <| ErrInfoGet e) }
                RemoteData.Success a -> { oldModel | auth = LoggedIn (User.updateInfo a user_) ns_ p_ }
                _ -> oldModel

updateUserInfo : SimpleInfo -> SharedModel -> SharedModel
updateUserInfo userInfo oldModel =
    case oldModel.auth of
        Guest _ -> oldModel
        LoggedIn user_ ns_ p_ ->
                { oldModel | auth = LoggedIn (User.updateInfo userInfo user_) ns_ p_ }

isGuest : SharedModel -> Bool
isGuest sm =
    case sm.auth of
        LoggedIn _ _ _  -> False
        _ -> True

user : SharedModel -> Maybe User
user sm =
    case sm.auth of
        LoggedIn val _ _ ->
            Just val
        Guest _ ->
            Nothing


isSubscribed : SharedModel -> Maybe Bool
isSubscribed sm =
    case sm.auth of
        LoggedIn u _ brN ->
            let
                browser = BrowserNotifications.isSubscribed brN
                telegram = User.info u |> .telegramId |> (==) Nothing |> not
            in
                Just (browser || telegram)
        Guest _ -> Nothing


login : User -> Cmd msg
login user_ =
    storeSession (Just (User.encode user_))

logout : Cmd msg
logout =
    storeSession Nothing

changes : (SharedModel -> msg) -> SharedModel -> Sub msg
changes toMsg sharedModel =
    onSessionChange (\val -> toMsg (decode sharedModel val))

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
                        BrowserNotifications.init


                Ok Nothing ->
                    Guest Nothing
                Err error ->
                    Guest (Just (ErrCacheParse error))
    in
    { oldModel | auth = newAuth}

port storeSession : Maybe Decode.Value -> Cmd msg
port onSessionChange : (Encode.Value -> msg) -> Sub msg

port dismissAlert : Decode.Value -> Cmd msg