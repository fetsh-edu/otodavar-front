module Profile exposing (..)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, h3, h5, img, span, text)
import Html.Attributes exposing (attribute, class, id, src)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Encode as Encode
import Login
import OtoApi exposing (config)
import Route
import SharedModel exposing (SharedModel)
import Url exposing (Url)
import User.Avatar as Avatar
import User.Bearer as Bearer
import User.FriendStatus exposing (Status(..))
import User.Name as Name
import User.Uid as Uid exposing (Uid)
import User.User as User exposing (User)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import View.Helper exposing (nbsp)

type alias Model =
    { session : SharedModel
    , uid : Uid
    , flow : WebData User.FullInfo
    , friendRequest : WebData User.FullInfo
    , confirmUnfriend : Maybe FriendRequest
    }

type alias FriendRequest
    = { friend : Uid, resource : Uid}

type Msg
    = HandleProfileResponse (WebData User.FullInfo)
    | HandleFriendRequest (WebData User.FullInfo)
    | AddFriendRequested FriendRequest
    | RemoveFriendRequested FriendRequest
    | AcceptFriendRequested FriendRequest
    | ConfirmUnfriend (Maybe FriendRequest)

init : SharedModel -> Uid -> (Model, Cmd Msg)
init session uid =
    ({session = session, uid = uid, flow = Loading, friendRequest = NotAsked, confirmUnfriend = Nothing}, get session uid)


updateSession : SharedModel -> Model -> Model
updateSession session model =
    { model | session  = session}

toSession : Model -> SharedModel
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
            ( {model | friendRequest = Loading, confirmUnfriend = Nothing}, friendRequestRemove (toSession model) obj )

        AcceptFriendRequested obj ->
            ( {model | friendRequest = Loading}, friendRequestAccept (toSession model) obj )

        ConfirmUnfriend fr ->
            ( { model | confirmUnfriend = fr }, Cmd.none )


friendRequest : SharedModel -> { friend : Uid, resource : Uid } -> Cmd Msg
friendRequest session {friend, resource} =
    let
        url = (OtoApi.routes (session.apiUrl)).friend.request { uid = friend, resource = (Just resource) }
        message bearer = RemoteData.Http.postWithConfig (OtoApi.config bearer) url HandleFriendRequest User.decoderFullInfo Encode.null
    in
    session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none



friendRequestAccept : SharedModel -> { friend : Uid, resource : Uid } -> Cmd Msg
friendRequestAccept session {friend, resource}  =
    let
        url = (OtoApi.routes session.apiUrl).friend.accept { uid = friend, resource = (Just resource) }
        message bearer = RemoteData.Http.postWithConfig (config bearer) url HandleFriendRequest User.decoderFullInfo Encode.null
    in
    session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


friendRequestRemove : SharedModel -> { friend : Uid, resource : Uid } -> Cmd Msg
friendRequestRemove session {friend, resource} =
    let
        url = (OtoApi.routes session.apiUrl).friend.remove { uid = friend, resource = (Just resource) }
        message bearer = RemoteData.Http.postWithConfig (config bearer) url HandleFriendRequest User.decoderFullInfo Encode.null
    in
    session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none

get : SharedModel -> Uid -> Cmd Msg
get session uid =
    let
        url = (OtoApi.routes session.apiUrl).profile uid
        message bearer = RemoteData.Http.getWithConfig (config bearer) url HandleProfileResponse User.decoderFullInfo
    in
    session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none

type alias Translator msg =
    { toSelf : Msg -> msg
    , onGameStart : Maybe Uid -> msg
    }

view : Translator msg -> Model -> Document msg
view translator model =
    { title
        = model.flow |> RemoteData.map (.name >> Name.toString) |> RemoteData.withDefault "Profile"

    , body =
        case model |> toSession |> SharedModel.user of
            Nothing -> [ text "SHOULDN'T BE POSSIBLE" ]
            Just me ->
                case model.flow of
                    Success pageUser -> [ successContent translator (model |> toSession) me pageUser model.confirmUnfriend]
                    NotAsked -> [View.Helper.loadingContainer "NOT ASKED"]
                    Loading -> [ loadingContent]
                    -- TODO: Handle all this
                    Failure e -> --
                        case e of
                            BadStatus 404 -> [ View.Helper.notFound ]
                            _ -> [View.Helper.simpleSmallContainer [text "ERROR"]]
    }


loadingContent : Html msg
loadingContent =
    let
        avatar =
            span
                [ class "filter drop-shadow-sm sm:drop-shadow-xl rounded-lg align-middle border-none"
                , class "absolute mt-0 md:-mt-16 md:w-40 w-20 md:h-40 h-20"
                , class "surface-variant"
                ]
                []
        fakeButton icon_ text_ =
            button
                [ class "font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none ease-linear transition-all duration-150"
                , class "filter drop-shadow"
                , class "primary primary-text"
                , class "px-4 py-2 m-1 mb-1"
                , Html.Attributes.disabled True
                ]
                [ span [ class "material-symbols-outlined md-18 mr-2" ][ text icon_ ]
                , text text_
                ]
        fakeCounter =
            div [ class "mr-0 sm:mr-4 px-3 sm:p-3 pt-0 pb-2 text-center"]
                [ span
                    [ class "text-xl font-bold sm:block uppercase tracking-wide text-slate-600"
                    , class "mr-1 sm:mr-0"
                    , class "surface-variant surface-variant-text rounded"
                    , class "px-2 sm:px-0"
                    ]
                    [ text nbsp ]
                , span
                    [ class "text-sm text-slate-400"
                    , class "surface-variant surface-variant-text rounded"
                    ]
                    [ text "Friends" ]
                ]
        actionButtons = [ fakeButton  "person" "remove", fakeButton  "sports_esports" "play" ]
        counters = [ fakeCounter, fakeCounter]
        title_ = h5 [ class "text-xl sm:text-2xl font-semibold leading-normal sm:mb-14 text-slate-700 mb-3 on-surface-variant-text"] [ text "Loading" ]
    in
    div [class "profile-page container mx-auto px-4 mt-6 md:mt-28 md:max-w-5xl"]
        [ profileHead (Just (class "animate-pulse")) avatar actionButtons counters title_
        ]





successContent : Translator msg -> SharedModel -> User -> User.FullInfo -> Maybe FriendRequest -> Html msg
successContent ({ toSelf, onGameStart } as translator) session me pageUser fr =
    let
        friendStatus = pageUser.friendStatus
        friendButton =
            if friendStatus == Me then
                shareButton pageUser session
            else
                 case friendStatus of
                     Me -> text ""
                     Unknown ->
                         actionButton { icon = "person_add", title = Just "Add", action = Just (toSelf (AddFriendRequested { friend = pageUser.uid, resource = pageUser.uid})), id_ = "add", shy = False }
                     Friend ->
                         actionButton { icon = "person_remove", title = Nothing, action = Just (toSelf (ConfirmUnfriend (Just { friend = pageUser.uid, resource = pageUser.uid}))), id_ = "remove", shy = True }
                     Requested ->
                         actionButton { icon = "hourglass_top", title = Just "Pending approval", action = Nothing, id_ = "remove", shy = False }
                     Wannabe ->
                         actionButton { icon = "person_add", title = Just "Accept", action = Just (toSelf (AcceptFriendRequested { friend = pageUser.uid, resource = pageUser.uid})), id_ = "remove", shy = False }
                    --
        playButton =
            if friendStatus == Friend then
                actionButton { icon = "sports_esports", title = Just "Play", action = Just <| onGameStart <| Just <| pageUser.uid, id_ = "play", shy = False }
            else
                text ""

        avatar =
            img
                [ attribute "referrerpolicy" "no-referrer"
                , src (Avatar.toString pageUser.avatar)
                , class "filter drop-shadow-sm sm:drop-shadow-xl rounded-lg align-middle border-none"
                , class "absolute mt-0 md:-mt-16 md:w-40 w-20 md:h-40 h-20"
                ]
                []
        actionButtons =
            case fr of
                Nothing ->
                    [ friendButton, playButton ]
                Just fr_ ->
                    [ actionButton { icon = "cancel", title = Just "Cancel", action = Just (toSelf (ConfirmUnfriend Nothing)), id_ = "remove", shy = False }
                    , actionButton { icon = "person_remove", title = Just "Remove", action = Just (toSelf (RemoveFriendRequested { friend = pageUser.uid, resource = pageUser.uid})), id_ = "remove", shy = False }
                    ]

        counters =
            [ counter pageUser.friendsCount "Friends"
            , counter pageUser.gamesCount "Games"
            ]

        title_ = h3 [ class "text-xl sm:text-4xl font-semibold leading-normal sm:mb-14 text-slate-700 mb-3"] [ text (Name.toString pageUser.name) ]

    in
    div [class "profile-page container mx-auto px-4 mt-6 md:mt-28 md:max-w-5xl"]
            [ profileHead Nothing avatar actionButtons counters title_
            , incomingRequests translator me pageUser
            , friendsList translator me pageUser
            , pendingApproval me pageUser
            ]



profileHead class_ avatar actionButtons counters title_ =
    div [ class "relative flex flex-col min-w-0 break-words w-full mb-6 shadow-xl rounded-lg surface-1 on-surface-text"
        , Maybe.withDefault (class "") class_
        ]
        [ div [ class "md:px-6 py-4 md:py-0"]
            [ div [ class "flex flex-row md:flex-col justify-between" ]
                [ div [ class "w-28 md:w-auto ml-4 md:ml-0 px-0 md:px-4 flex justify-left md:justify-center"]
                    [ avatar ]
                , div [ class "w-full flex flex-col sm:flex-row"]
                    [ div [ class "flex flex-row justify-center md:justify-end text-right self-center"
                          , class "w-full md:w-5/12 order-3 mt-0 md:h-28 items-center"
                          , class "py-0 sm:pr-4"
                          ]
                        actionButtons
                    , div [class "w-2/12 px-4 order-2"] []
                    , div
                        [ class "flex mt-0 w-full md:w-5/12 px-4 order-1 justify-center md:justify-start"
                        , class "py-0 sm:py-0 md:py-3"
                        ]
                        counters
                    ]
                ]
            , div [ class "text-center mt-3 sm:mt-12" ]
                [ title_
                ]
            ]
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
                , shy = False
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
        friendStatus = pageUser.friendStatus

        actionButton_ : User.SimpleInfo -> Html msg
        actionButton_ user =
            case user.friendStatus of
                Me ->
                    text ""
                Friend ->
                    actionButton
                        { icon = "sports_esports"
                        , title = Nothing
                        , action = Just <| onGameStart <| Just <| user.uid
                        , id_ = "play" ++ (Uid.toString user.uid)
                        , shy = False
                        }
                Requested ->
                    actionButton
                        { icon = "hourglass_top"
                        , title = Nothing
                        , action = Nothing
                        , id_ = "pending" ++ (Uid.toString user.uid)
                        , shy = False
                        }
                Wannabe ->
                    actionButton
                        { icon = "person_add"
                        , title = Nothing
                        , action = Just (toSelf (AcceptFriendRequested {friend = user.uid, resource = pageUser.uid}))
                        , id_ = "accept" ++ (Uid.toString user.uid)
                        , shy = False
                        }
                Unknown ->
                    actionButton
                        { icon = "person_add"
                        , title = Nothing
                        , action = Just (toSelf (AddFriendRequested { friend = user.uid, resource = pageUser.uid}))
                        , id_ = "add_friend" ++ (Uid.toString user.uid)
                        , shy = False
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
         [ class "relative flex flex-col min-w-0 break-words surface on-surface-text w-full mb-6 shadow-xl rounded-lg surface-1 on-surface-text"]
         [ div
            [ class "rounded-t-lg py-2 px-4 font-bold"
            , class class_
            ]
            [ text title_ ]
         , div [ class "divide-y divide-light"] (List.map (friendView actionButton_) users)
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
        rootUrl = session |> SharedModel.url |> Login.rootUrl
        route = Url.toString {rootUrl | path = (userInfo |> .uid |> Route.Profile |> Route.routeToString)}
    in
    Html.node "clipboard-copy"
        [ Html.Attributes.value route, class "flex w-10 w-10 primary on-primary-text min-w-min"
        , class "primary on-primary-text font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none ease-linear transition-all duration-150"
        , class "whitespace-nowrap px-4 py-2 m-1 mb-1"
        , class "filter drop-shadow"
        , class "cursor-pointer"
        ]
        [ span
            [ class "material-symbols-outlined md-18 mr-2" ]
            [ text "share"]
        , text "Copy URL"
        ]

actionButton : { a | icon : String, title : Maybe String, action : Maybe msg, id_ : String, shy : Bool } -> Html msg
actionButton {icon, title, action, id_, shy} =
    let
        disabled_ =
            case action of
                Nothing -> True
                Just _ -> False
        colors =
            if disabled_ || shy then
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
            ([ class "font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none"
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