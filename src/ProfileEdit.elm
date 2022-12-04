module ProfileEdit exposing (..)

import Browser exposing (Document)
import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes exposing (attribute, autofocus, class, src, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Notifications.BrowserNotifications exposing (BrowserNotifications)
import Notifications.PushPermission as PushPermission
import OtoApi
import Push exposing (Push)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import SharedModel exposing (SharedModel, updateUserInfo)
import User.Avatar as Avatar
import User.Bearer as Bearer
import User.Email as Email
import User.Name as Name
import User.User as User
import View.Helper


type alias Model =
    { sharedModel : SharedModel
    , newName : String
    , logoutAsked : Bool
    , saveResult : WebData User.SimpleInfo
    }

init : SharedModel -> (Model, Cmd Msg)
init sharedModel =
    ( { sharedModel = sharedModel
      , newName = sharedModel |> SharedModel.user |> Maybe.map (User.info >> .name >> Name.toString) |> Maybe.withDefault ""
      , logoutAsked = False
      , saveResult = NotAsked
      }
    , Cmd.none
    )

type Msg
    = NoOp
    | OnUserNameChange String
    | Save
    | HandleSaved (WebData User.SimpleInfo)
    | ConfirmLogout
    | DeclineLogout
    | Logout

type alias Translator msg =
    { toSelf : Msg -> msg
    , toParent : Notifications.BrowserNotifications.Msg -> msg
    }



view : Translator msg -> Model -> Document msg
view translator model =
    { title = "Edit profile"
    , body =
        case model |> .sharedModel |> SharedModel.user of
            Nothing -> [ text "SHOULDN'T BE POSSIBLE" ]
            Just me -> [ successView translator (User.info me) model.newName model.logoutAsked model.sharedModel ]
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        OnUserNameChange str ->
            ( { model | newName = str }, Cmd.none )
        Save ->
            ( model, save (model.sharedModel) (model.newName) )
        HandleSaved a ->
            let
                newModel = { model | saveResult = a }
            in
            case a of
                RemoteData.Success newInfo ->
                    ({ newModel | sharedModel = model.sharedModel |> updateUserInfo newInfo, newName = newInfo.name |> Name.toString }, Cmd.none)
                _ -> (newModel, Cmd.none)
        ConfirmLogout ->
            ( { model | logoutAsked = True }, Cmd.none )
        DeclineLogout ->
            ( { model | logoutAsked = False }, Cmd.none )
        Logout ->
            ( model, SharedModel.logout )


save : SharedModel -> String -> Cmd Msg
save sharedModel name =
    let
        url = (OtoApi.routes (sharedModel.apiUrl)).update_profile
        payload = Encode.object [ ( "name", Encode.string name )]
        message bearer = RemoteData.Http.postWithConfig (OtoApi.config bearer) url HandleSaved User.decoderInfo payload
    in
    sharedModel |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none

successView : Translator msg -> { a | avatar : Avatar.Avatar, email : Email.Email } -> String -> Bool -> SharedModel -> Html.Html msg
successView translator me newName logoutAsked sharedModel =
    div [ class "profile-page container mx-auto px-4 mt-8 md:mt-28 md:max-w-5xl"]
        [ div
            [ class "relative flex flex-col min-w-0 break-words w-full mb-6 shadow-xl rounded-lg surface-1 on-surface-text" ]
            [ div
                [ class "px-6 py-4"]
                [ div
                    [ class "w-auto ml-0 px-4 flex justify-center"]
                    [ img
                        [ attribute "referrerpolicy" "no-referrer"
                        , src (Avatar.toString me.avatar)
                        , class "filter drop-shadow-sm sm:drop-shadow-xl rounded-lg align-middle border-none"
                        , class "-mt-10 md:-mt-16 md:w-40 w-20 md:h-40 h-20"
                        ]
                        []
                    ]
                , div
                    [ class "flex flex-row items-baseline mt-4" ]
                    [ span [class "text-xs w-20 capitalize"] [text "Username"]
                    , input
                        [ value newName
                        , style "appearance" "none"
                        , class "bg-transparent focus:outline-none w-44 border-b-2 py-1 border-dashed"
                        , autofocus True
                        , onInput (translator.toSelf << OnUserNameChange)
                        ]
                        []
                    ]
                , div
                    [ class "flex flex-row items-baseline mt-4" ]
                    [ span [class "text-xs w-20 capitalize"] [text "email"], text (Email.toString me.email) ]
                , if logoutAsked
                    then
                        div
                            [ class "flex flex-row items-center mt-8" ]
                            [ span [ class "mr-4"] [ text "Are you sure?" ]
                            , button
                                [ class "font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none"
                                , class "primary on-primary-text"
                                , class "filter drop-shadow"
                                , class "px-4 h-10"
                                , onClick (translator.toSelf DeclineLogout)
                                ]
                                [ text "Cancel"
                                ]
                            , button
                                [ class "font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none"
                                , class "surface-1 error-text"
                                , class "border border-red-700"
                                , class "filter drop-shadow"
                                , class "px-4 h-10"
                                , onClick (translator.toSelf Logout)
                                ]
                                [ span [ class "text-base material-symbols-outlined mr-2" ] [ text "exit_to_app"]
                                , text "Log out"
                                ]
                            ]
                    else
                        div
                            [ class "flex flex-row items-center mt-8" ]
                            [ button
                                [ class "font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none"
                                , class "surface-1 error-text"
                                , class "border border-red-700"
                                , class "filter drop-shadow"
                                , class "px-4 h-10"
                                , onClick (translator.toSelf ConfirmLogout)
                                ]
                                [ text "Log out"
                                ]
                            , button
                                [ class "font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none"
                                , class "primary on-primary-text"
                                , class "filter drop-shadow"
                                , class "px-4 h-10"
                                , onClick (translator.toSelf Save)
                                ]
                                [ span [ class "text-base material-symbols-outlined mr-2" ][ text "save_as"]
                                , text "Save"
                                ]
                            ]
                ]
            ]
        , View.Helper.section "Notifications" "secondary-container on-secondary-container-text uppercase text-center"
            [ div
                [ class "pt-2 px-2 pt-4 pb-2"]
                [ div [ class "text-sm on-surface-variant-text pb-2"] [ text "System notifications"]
                , div [] [ systemNotifications translator (SharedModel.browserNotifications sharedModel) ]
                ]
            , div [class "mt-2 pt-2 px-2"] [ text "Telegram notifications" ]
            ]
        ]

systemNotifications : Translator msg -> Maybe BrowserNotifications -> Html msg
systemNotifications translator some =
    case some of
        Just browserNotifications ->
            case browserNotifications.permission of
                Ok permission ->
                    div []
                        [ case permission of
                            PushPermission.Granted ->
                                div
                                    []
                                    [ div [ class "pt-2 pb-4 text-sm" ] [text "Permission to receive notifications is granted."]
                                    , div [] [subscribeButton browserNotifications.push translator]
                                    ]
                            PushPermission.Denied -> span [ class "pt-2 pb-4 text-sm" ] [ text "Permission to receive notifications is denied. There is nothing we can do at this stage. You need to grant notifications permission somewhere in your browser."]
                            PushPermission.Default ->
                                span
                                    [ class "flex relative items-center rounded-md drawer-item h-10 px-2 py-6 mx-2 mb-2 cursor-pointer"
                                    , onClick (translator.toParent Notifications.BrowserNotifications.RequestPermission)
                                    ]
                                    [ span [ class "material-symbols-outlined mr-4" ] [ text "chat_bubble" ]
                                    , text "Ask for permission"
                                    ]
                            PushPermission.NotAsked ->
                                span
                                    [ class "flex relative items-center rounded-md drawer-item h-10 px-2 py-6 mx-2 mb-2 cursor-pointer"
                                    , onClick (translator.toParent Notifications.BrowserNotifications.RequestPermission)
                                    ]
                                    [ span [ class "material-symbols-outlined mr-4" ] [ text "chat_bubble" ]
                                    , text "Ask for permission"
                                    ]
                            PushPermission.NotSupported -> span [class "pt-2 pb-4 text-sm"] [ text "Push notifications are not supported in this browser" ]
                            PushPermission.Error e -> span [class "pt-2 pb-4 text-sm"] [text ("Sliha, We couldn't receive your permission. " ++ e)]
                        ]

                Err error ->
                    text "Sliha, we can't get your notification permission"
        Nothing ->
            text ""


subscribeButton : Push -> Translator msg -> Html msg
subscribeButton push translator =
    case push of
        Push.NotAsked ->
            span
                [ class "flex relative items-center rounded-md drawer-item h-10 px-2 py-6 mx-2 mb-2 cursor-pointer"
                , onClick (translator.toParent Notifications.BrowserNotifications.Subscribe)
                ]
                [ span [ class "material-symbols-outlined mr-4" ] [ text "notification_add" ]
                , text "Subscribe"
                ]
        Push.Error string -> text "not asked"
        Push.Subscribed string ->
            span
                [ class "flex relative items-center rounded-md drawer-item h-10 px-2 py-6 mx-2 mb-2 cursor-pointer"
                , onClick (translator.toParent Notifications.BrowserNotifications.Unsubscribe)
                ]
                [ span [ class "material-symbols-outlined mr-4" ] [ text "notifications_off" ]
                , text "Unsubscribe"
                ]
        Push.Unsubscribed maybeString ->
            span
                [ class "flex relative items-center rounded-md drawer-item h-10 px-2 py-6 mx-2 mb-2 cursor-pointer"
                , onClick (translator.toParent Notifications.BrowserNotifications.Subscribe)
                ]
                [ span [ class "material-symbols-outlined mr-4" ] [ text "notification_add" ]
                , text "Subscribe"
                ]
        --                subscribePush icon_ t_ =
        --                    span
        --                        [ class "flex relative items-center rounded-md drawer-item h-10 px-2 py-6 mx-2 mb-2"
        --                        , class cursor
        --                        , onClick (GotPushMsg Push.Subscribe)
        --                        , disabled p_.buttonDisabled
        --                        ]
        --                        [ span [ class "material-symbols-outlined mr-4" ] [ text icon_]
        --                        , text t_
        --                        ]
        --                unsubscribePush t_ =
        --                    span
        --                        [ class "flex relative items-center rounded-md cursor-pointer drawer-item h-10 px-2 py-6 mx-2 mb-2"
        --                        , class cursor
        --                        , onClick (GotPushMsg Push.UnSubscribe)
        --                        , disabled p_.buttonDisabled
        --                        ]
        --                        [ span [ class "material-symbols-outlined mr-4" ] [ text "notifications_off"]
        --                        , text t_
        --                        ]
        --                pushButton =
        --                    case  p_.state of
        --                        Push.NotAsked ->
        --                            subscribePush "notification_add" "Turn on"
        --                        Push.Unsubscribed _->
        --                            subscribePush "notification_add" "Turn on"
        --                        Push.Denied ->
        --                            subscribePush "notifications_paused" "Blocked for this site"
        --                        Push.Error a ->
        --                            subscribePush "notification_important" ("Error" )
        --                        Push.NotSupported ->
        --                            text ""
        --                        Push.Subscribed _ ->
        --                            unsubscribePush "Turn off"

