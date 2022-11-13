module Profile exposing (..)

import Html exposing (Html, a, button, div, h3, img, main_, span, text)
import Html.Attributes exposing (attribute, class, disabled, id, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Encode as Encode
import Login exposing (cleanUrl)
import OtoApi exposing (config, url)
import Route
import Session exposing (Session)
import Url
import User.Avatar as Avatar
import User.Bearer as Bearer
import User.Config exposing (defaultHttpsUrl)
import User.FriendStatus exposing (Status(..))
import User.Name as Name
import User.Uid as Uid exposing (Uid)
import User.User as User exposing (User)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (defaultConfig)

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
        path = "/api/v1/users/" ++ (Uid.toString uid)
    in
    case session |> Session.bearer |> Maybe.map Bearer.toString of
        Nothing  -> Cmd.none
        Just bearer ->
            RemoteData.Http.getWithConfig (config bearer) (url path Nothing) HandleProfileResponse User.decoderFullInfo

view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Profile"
    , content =
        case model |> toSession |> Session.user of
            Nothing -> text "SHOULDN'T BE POSSIBLE"
            Just me ->
                case model.flow of
                    Success pageUser -> successContent (model |> toSession) me pageUser
                    NotAsked -> text "NOT ASKED"
                    Loading -> container "LOADING"
                    Failure e -> text "ERROR"
    }


container text_ =
    div
        [ class "flex flex-col m-10 justify-center items-center"]
        [ div
            [ class "transition-transform transform w-full md:w-1/2 surface-1 on-surface-text rounded-lg flex flex-row p-4 mb-8 text-lg shadow-md justify-center items-center" ]
            [ span
                [ class "animate-spin flex justify-center items-center h-14 w-14 material-icons mr-0" ]
                [ text "refresh"]
            , span [ class "pl-0 overflow-ellipsis overflow-hidden"] [text text_]
            ]
        ]



successContent : Session -> User -> User.FullInfo -> Html Msg
successContent session me pageUser =
    let
        friendButton =
            if pageUser.uid == (User.info me).uid then
                shareButton pageUser session
            else
                 case pageUser.friendStatus of
                     Me -> text ""
                     Unknown ->
                         actionButton { icon = "person_add", title = Just "Add", action = Just (AddFriendRequested { friend = pageUser.uid, resource = pageUser.uid}), id_ = "add" }
                     Friend ->
                         actionButton { icon = "person_remove", title = Just "Remove", action = Just (RemoveFriendRequested { friend = pageUser.uid, resource = pageUser.uid}), id_ = "remove" }
                     Requested ->
                         actionButton { icon = "hourglass_top", title = Just "Pending approval", action = Nothing, id_ = "remove" }
                     Wannabe ->
                         actionButton { icon = "person_add", title = Just "Accept", action = Just (AcceptFriendRequested { friend = pageUser.uid, resource = pageUser.uid}), id_ = "remove" }
                    --
        playButton =
            if pageUser.friendStatus == Friend then
                actionButton { icon = "sports_esports", title = Just "Play", action = Nothing, id_ = "play" }
            else
                text ""
    in
    div [class "profile-page container mx-auto px-4 mt-10 md:mt-28 md:max-w-5xl"]
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
            , incomingRequests me pageUser
            , friendsList me pageUser
            , pendingApproval me pageUser
            ]


incomingRequests : User -> User.FullInfo -> Html Msg
incomingRequests me pageUser =
    let
        actionButton_ user =
            actionButton
                { icon = "person_add"
                , title = Nothing
                , action = Just (AcceptFriendRequested {friend = user.uid, resource = pageUser.uid})
                , id_ = "accept" ++ (Uid.toString user.uid)
                }
    in
    if List.length pageUser.incomingFriendRequests > 0 && (pageUser.uid == (User.info me).uid) then
        userList actionButton_ "Friend requests" "secondary-container on-secondary-container-text" pageUser.incomingFriendRequests
    else
        text ""


pendingApproval : User -> User.FullInfo -> Html msg
pendingApproval me other =
    if List.length other.outgoingFriendRequests > 0 && (other.uid == (User.info me).uid) then
        userList (\x -> text "") "Pending" "error-container on-error-container-text" other.outgoingFriendRequests
    else
        text ""

friendsList : User -> User.FullInfo -> Html Msg
friendsList me pageUser =
    let
        actionButton_ : User.Info -> Html Msg
        actionButton_ user =
            case user.friendStatus of
                Me ->
                    text ""
                Friend ->
                    actionButton
                        { icon = "sports_esports"
                        , title = Nothing
                        , action = Nothing
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
                        , action = Just (AcceptFriendRequested {friend = user.uid, resource = pageUser.uid})
                        , id_ = "accept" ++ (Uid.toString user.uid)
                        }
                Unknown ->
                    actionButton
                        { icon = "person_add"
                        , title = Nothing
                        , action = Just (AddFriendRequested { friend = user.uid, resource = pageUser.uid})
                        , id_ = "add_friend" ++ (Uid.toString user.uid)
                        }

    in
    if List.length pageUser.friends > 0 && (pageUser.uid == (User.info me).uid || pageUser.friendStatus == Friend) then
        userList actionButton_ "Friends" "tertiary-container on-tertiary-container-text" pageUser.friends
    else
        text ""


userList : (User.Info -> Html msg) -> String -> String -> List User.Info -> Html msg
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


friendView : (User.Info -> Html msg) -> User.Info -> Html msg
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
            [ class "material-icons md-18 mr-2" ]
            [ text "share"]
        , text "Copy URL"
        ]

actionButton : { a | icon : String, title : Maybe String, action : Maybe Msg, id_ : String } -> Html Msg
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
            [ span [ class "material-icons md-18", class mrClass ][ text icon]
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