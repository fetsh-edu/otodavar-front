module Profile exposing (..)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, h3, img, span, text)
import Html.Attributes exposing (attribute, class, id, src)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Login
import OtoApi exposing (config, url)
import Route
import Session exposing (Session)
import Url
import User.Avatar as Avatar
import User.Bearer as Bearer
import User.FriendStatus exposing (Status(..))
import User.Name as Name
import User.Uid as Uid exposing (Uid)
import User.User as User exposing (User)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import View.Helper

type alias Model =
    { session : Session
    , uid : Uid
    , flow : WebData User.FullInfo
    , friendRequest : WebData User.FullInfo
    }

type Msg
    = HandleProfileResponse (WebData User.FullInfo)
    | HandleFriendRequest (WebData User.FullInfo)
    | AddFriendRequested { friend : Uid, resource : Uid}
    | RemoveFriendRequested { friend : Uid, resource : Uid}
    | AcceptFriendRequested { friend : Uid, resource : Uid}

init : Session -> Uid -> (Model, Cmd Msg)
init session uid =
    ({session = session, uid = uid, flow = Loading, friendRequest = NotAsked}, get session uid)


updateSession : Session -> Model -> Model
updateSession session model =
    { model | session  = session}

toSession : Model -> Session
toSession { session } = session

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleProfileResponse response ->
            ({model | flow = response }, Cmd.none)

        AddFriendRequested obj ->
            ( {model | friendRequest = Loading}, friendRequest (toSession model) obj )

        HandleFriendRequest webData ->
            case model.flow of
                Success some ->
                    ( {model | friendRequest = webData, flow = webData }, Cmd.none)
                _ ->
                    (model, Cmd.none)

        RemoveFriendRequested obj ->
            ( {model | friendRequest = Loading}, friendRequestRemove (toSession model) obj )

        AcceptFriendRequested obj ->
            ( {model | friendRequest = Loading}, friendRequestAccept (toSession model) obj )


friendRequest : Session -> { friend : Uid, resource : Uid } -> Cmd Msg
friendRequest session {friend, resource} =
    let
        url = OtoApi.routes.friend.request { uid = friend, resource = (Just resource) }
        message bearer = RemoteData.Http.postWithConfig (OtoApi.config bearer) url HandleFriendRequest User.decoderFullInfo Encode.null
    in
    session |> Session.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none



friendRequestAccept : Session -> { friend : Uid, resource : Uid } -> Cmd Msg
friendRequestAccept session {friend, resource}  =
    let
        url = OtoApi.routes.friend.accept { uid = friend, resource = (Just resource) }
        message bearer = RemoteData.Http.postWithConfig (config bearer) url HandleFriendRequest User.decoderFullInfo Encode.null
    in
    session |> Session.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


friendRequestRemove : Session -> { friend : Uid, resource : Uid } -> Cmd Msg
friendRequestRemove session {friend, resource} =
    let
        url = OtoApi.routes.friend.remove { uid = friend, resource = (Just resource) }
        message bearer = RemoteData.Http.postWithConfig (config bearer) url HandleFriendRequest User.decoderFullInfo Encode.null
    in
    session |> Session.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none

get : Session -> Uid -> Cmd Msg
get session uid =
    let
        url = OtoApi.routes.profile uid
        message bearer = RemoteData.Http.getWithConfig (config bearer) url HandleProfileResponse User.decoderFullInfo
    in
    session |> Session.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none

type alias Translator msg =
    { toSelf : Msg -> msg
    , onGameStart : Maybe Uid -> msg
    }

view : Translator msg -> Model -> Document msg
view translator model =
    { title
        = model.flow |> RemoteData.map (.name >> Name.toString) |> RemoteData.withDefault "Profile"

    , body =
        case model |> toSession |> Session.user of
            Nothing -> [ text "SHOULDN'T BE POSSIBLE" ]
            Just me ->
                case model.flow of
                    Success pageUser -> [ successContent translator (model |> toSession) me pageUser]
                    NotAsked -> [View.Helper.smallContainer "NOT ASKED"]
                    Loading -> [View.Helper.smallContainer "LOADING"]
                    -- TODO: Handle all this
                    Failure e -> [View.Helper.smallContainer "ERROR"]
    }




successContent : Translator msg -> Session -> User -> User.FullInfo -> Html msg
successContent ({ toSelf, onGameStart } as translator) session me pageUser =
    let
        friendStatus = User.friendStatus me pageUser.uid
        friendButton =
            if friendStatus == Me then
                shareButton pageUser session
            else
                 case friendStatus of
                     Me -> text ""
                     Unknown ->
                         actionButton { icon = "person_add", title = Just "Add", action = Just (toSelf (AddFriendRequested { friend = pageUser.uid, resource = pageUser.uid})), id_ = "add" }
                     Friend ->
                         actionButton { icon = "person_remove", title = Just "Remove", action = Just (toSelf (RemoveFriendRequested { friend = pageUser.uid, resource = pageUser.uid})), id_ = "remove" }
                     Requested ->
                         actionButton { icon = "hourglass_top", title = Just "Pending approval", action = Nothing, id_ = "remove" }
                     Wannabe ->
                         actionButton { icon = "person_add", title = Just "Accept", action = Just (toSelf (AcceptFriendRequested { friend = pageUser.uid, resource = pageUser.uid})), id_ = "remove" }
                    --
        playButton =
            if friendStatus == Friend then
                actionButton { icon = "sports_esports", title = Just "Play", action = Just <| onGameStart <| Just <| pageUser.uid, id_ = "play" }
            else
                text ""
    in
    div [class "profile-page container mx-auto px-4 mt-6 md:mt-28 md:max-w-5xl"]
            [ div [ class "relative flex flex-col min-w-0 break-words bg-white w-full mb-6 shadow-xl rounded-lg surface-1 on-surface-text"]
                [ div [ class "px-4 md:px-6 py-4 md:py-0"]
                    [ div [ class "flex flex-row md:flex-col justify-between" ]
                        [ div [ class "w-28 md:w-full px-0 md:px-4 flex justify-left md:justify-center"]
                            [ img
                                [ attribute "referrerpolicy" "no-referrer"
                                , src (Avatar.toString pageUser.avatar)
                                , class "filter drop-shadow-sm sm:drop-shadow-xl rounded-lg align-middle border-none"
                                , class "absolute mt-0 md:-mt-16 md:w-40 w-20 md:h-40 h-20"
                                ]
                                []
                            ]
                        , div [ class "w-full flex flex-col sm:flex-row"]
                            [ div [ class "flex flex-row justify-center md:justify-end text-right self-center"
                                  , class "w-full md:w-5/12 order-3 px-3 mt-0"
                                  , class "py-0 md:py-6"
                                  ]
                                [ friendButton
                                , playButton
                                ]
                            , div [class "w-2/12 px-4 order-2"] []
                            , div
                                [ class "flex mt-0 w-full md:w-5/12 px-4 order-1 justify-center md:justify-start"
                                , class "py-0 sm:py-0 md:py-3"
                                ]
                                [ counter pageUser.friendsCount "Friends"
                                , counter pageUser.gamesCount "Games"
                                ]
                            ]
                        ]
                    , div [ class "text-center mt-3 sm:mt-12" ]
                        [ h3 [ class "text-xl sm:text-4xl font-semibold leading-normal sm:mb-14 text-slate-700 mb-3"] [ text (Name.toString pageUser.name) ]
                        ]
                    ]
                ]
            , incomingRequests translator me pageUser
            , friendsList translator me pageUser
            , pendingApproval me pageUser
            ]


incomingRequests : Translator msg -> User -> User.FullInfo -> Html msg
incomingRequests { toSelf } me pageUser =
    let
        actionButton_ user =
            actionButton
                { icon = "person_add"
                , title = Nothing
                , action = Just (toSelf (AcceptFriendRequested {friend = user.uid, resource = pageUser.uid}))
                , id_ = "accept" ++ (Uid.toString user.uid)
                }
    in
    case pageUser.incomingFriendRequests of
        Nothing -> text ""
        Just users ->
            if List.length users > 0 && (pageUser.uid == (User.info me).uid) then
                userList actionButton_ "Friend requests" "secondary-container on-secondary-container-text" users
            else
                text ""


pendingApproval : User -> User.FullInfo -> Html msg
pendingApproval me other =
    case other.outgoingFriendRequests of
        Nothing -> text ""
        Just users ->
            if List.length users > 0 && (other.uid == (User.info me).uid) then
                userList (\x -> text "") "Pending" "error-container on-error-container-text" users
            else
                text ""


friendsList : Translator msg -> User -> User.FullInfo -> Html msg
friendsList ( { toSelf, onGameStart } as translator) me pageUser =
    let
        friendStatus : Status
        friendStatus = User.friendStatus me pageUser.uid

        actionButton_ : User.SimpleInfo -> Html msg
        actionButton_ user =
            case User.friendStatus me user.uid of
                Me ->
                    text ""
                Friend ->
                    actionButton
                        { icon = "sports_esports"
                        , title = Nothing
                        , action = Just <| onGameStart <| Just <| user.uid
                        , id_ = "play" ++ (Uid.toString user.uid)
                        }
                Requested ->
                    actionButton
                        { icon = "hourglass_top"
                        , title = Nothing
                        , action = Nothing
                        , id_ = "pending" ++ (Uid.toString user.uid)
                        }
                Wannabe ->
                    actionButton
                        { icon = "person_add"
                        , title = Nothing
                        , action = Just (toSelf (AcceptFriendRequested {friend = user.uid, resource = pageUser.uid}))
                        , id_ = "accept" ++ (Uid.toString user.uid)
                        }
                Unknown ->
                    actionButton
                        { icon = "person_add"
                        , title = Nothing
                        , action = Just (toSelf (AddFriendRequested { friend = user.uid, resource = pageUser.uid}))
                        , id_ = "add_friend" ++ (Uid.toString user.uid)
                        }

    in
    case pageUser.friends of
        Nothing -> text ""
        Just users ->
            if List.length users > 0 && (friendStatus == Me || friendStatus == Friend) then
                userList actionButton_ "Friends" "tertiary-container on-tertiary-container-text" users
            else
                text ""


userList : (User.SimpleInfo -> Html msg) -> String -> String -> List User.SimpleInfo -> Html msg
userList actionButton_ title_ class_ users =
     div
         [ class "relative flex flex-col min-w-0 break-words bg-white w-full mb-6 shadow-xl rounded-lg surface-1 on-surface-text"]
         [ div
            [ class "rounded-t-lg py-2 px-4 font-bold"
            , class class_
            ]
            [ text title_ ]
         , div [ class "divide-y divide-pink-200"] (List.map (friendView actionButton_) users)
         ]


friendView : (User.SimpleInfo -> Html msg) -> User.SimpleInfo -> Html msg
friendView actionButton_ other =
     div
        [ class "flex flex-row items-center"]
        [ a
            [ Route.href (other |> .uid |> Route.Profile)
            , class "h-12 w-12 m-2"
            ]
            [ img
                [ src (Avatar.toString other.avatar)
                , attribute "referrerpolicy" "no-referrer"
                , class "rounded-lg"
                ] []
            ]
        , a
            [ Route.href (other |> .uid |> Route.Profile)
            , class "ml-2 flex-1 py-3"
            ]
            [ span
                []
                [text (Name.toString other.name)]
            ]
        , span
            [ class "mr-2"]
            [ actionButton_ other
            ]
        ]

shareButton userInfo session =
    let
        rootUrl = session |> Session.url |> Login.rootUrl
        route = Url.toString {rootUrl | path = (userInfo |> .uid |> Route.Profile |> Route.routeToString)}
    in
    Html.node "clipboard-copy"
        [ Html.Attributes.value route, class "flex w-10 w-10 primary on-primary-text min-w-min"
        , class "primary on-primary-text text-white font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none ease-linear transition-all duration-150"
        , class "whitespace-nowrap px-4 py-2 m-1 mb-1"
        , class "filter drop-shadow"
        ]
        [ span
            [ class "material-symbols-outlined md-18 mr-2" ]
            [ text "share"]
        , text "Copy URL"
        ]

actionButton : { a | icon : String, title : Maybe String, action : Maybe msg, id_ : String } -> Html msg
actionButton {icon, title, action, id_} =
    let
        disabled_ =
            case action of
                Nothing -> True
                Just _ -> False
        colors =
            if disabled_ then
                "background on-background-text"
            else
                "primary on-primary-text"
        pClass =
            case title of
                Nothing -> "px-3 py-2 m-1 mb-1"
                Just _ -> "px-4 py-2 m-1 mb-1"
        mrClass =
            case title of
                Nothing -> "mr-0"
                Just _ -> "mr-2"
    in
        button
            ([ class "font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none ease-linear transition-all duration-150"
            , class "filter drop-shadow"
            , class colors, class pClass
            , id id_
            , Html.Attributes.disabled disabled_
            ] ++ (action |> Maybe.map (List.singleton << onClick) |> Maybe.withDefault []))
            [ span [ class "material-symbols-outlined md-18", class mrClass ][ text icon]
            , title |> Maybe.withDefault "" |> text
            ]

counter : Int -> String -> Html msg
counter int label =
    div [ class "mr-0 sm:mr-4 px-3 sm:p-3 pt-0 pb-2 text-center"]
        [ span
            [ class "text-xl font-bold sm:block uppercase tracking-wide text-slate-600"
            , class "mr-1 sm:mr-0"
            ]
            [ text (String.fromInt int) ]
        , span [ class "text-sm text-slate-400" ]
            [ text label ]
        ]