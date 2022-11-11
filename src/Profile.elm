module Profile exposing (..)

import Html exposing (Html, a, button, div, h3, img, main_, span, text)
import Html.Attributes exposing (attribute, class, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Encode as Encode
import Route
import Session exposing (Session)
import Url
import User.Avatar as Avatar
import User.Bearer as Bearer
import User.Config exposing (defaultHttpsUrl)
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
    | AddFriendRequested User.FullInfo
    | HandleFriendRequest (WebData User.FullInfo)
    | RemoveFriendRequested User.FullInfo

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

        AddFriendRequested info ->
            ( {model | friendRequest = Loading}, friendRequest (toSession model) info )

        HandleFriendRequest webData ->
            ( {model | friendRequest = webData, flow = webData }, Cmd.none)

        RemoveFriendRequested info ->
            ( {model | friendRequest = Loading}, friendRequestRemove (toSession model) info )


friendRequest : Session -> User.FullInfo -> Cmd Msg
friendRequest session info =
    let
        path = info |> .uid |> Uid.toString |> (\x -> "/api/v1/users/" ++ x ++ "/friend")
        url = Url.toString {defaultHttpsUrl | host = "localhost", path = path, port_ = Just 3001}
        config bearer = { defaultConfig | headers = [(Http.header "Authorization" bearer), (Http.header "Accept" "application/json")] }
    in
    case session |> Session.bearer |> Maybe.map Bearer.toString of
        Nothing  -> Cmd.none
        Just bearer ->
            RemoteData.Http.postWithConfig (config bearer) url HandleFriendRequest User.decoderFullInfo Encode.null


friendRequestRemove : Session -> User.FullInfo -> Cmd Msg
friendRequestRemove session info =
    let
        path = info |> .uid |> Uid.toString |> (\x -> "/api/v1/users/" ++ x ++ "/unfriend")
        url = Url.toString {defaultHttpsUrl | host = "localhost", path = path, port_ = Just 3001}
        config bearer = { defaultConfig | headers = [(Http.header "Authorization" bearer), (Http.header "Accept" "application/json")] }
    in
    case session |> Session.bearer |> Maybe.map Bearer.toString of
        Nothing  -> Cmd.none
        Just bearer ->
            RemoteData.Http.postWithConfig (config bearer) url HandleFriendRequest User.decoderFullInfo Encode.null



get : Session -> Uid -> Cmd Msg
get session uid =
    let
        url = Url.toString {defaultHttpsUrl | host = "localhost", path = "/api/v1/users/" ++ (Uid.toString uid), port_ = Just 3001}
    in
    case session |> Session.bearer |> Maybe.map Bearer.toString of
        Nothing  -> Cmd.none
        Just bearer ->
            RemoteData.Http.getWithConfig { defaultConfig | headers = [(Http.header "Authorization" bearer)] } url HandleProfileResponse User.decoderFullInfo

view : Model -> { title : String, content : Html Msg }
view model =

    { title = "Profile"
    , content =
        case model |> toSession |> Session.user of
            Nothing -> text "SHOULDN'T BE POSSIBLE"
            Just me ->
                case model.flow of
                    Success other -> successContent me other
                    NotAsked -> text "NOT ASKED"
                    Loading -> text "LOADING"
                    Failure e -> text "ERROR"
    }

successContent : User -> User.FullInfo -> Html Msg
successContent me other =
    let
        friendButton =
            if other.uid == (User.info me).uid then
                actionButton { icon = "share", title = "Share", action = AddFriendRequested other }
            else
                 if other.isFriend then
                    actionButton { icon = "person_remove", title = "Remove", action = RemoveFriendRequested other }
                 else
                    actionButton { icon = "person_add", title = "Add", action = AddFriendRequested other }
        playButton =
            if other.uid == (User.info me).uid then
                text ""
            else
                actionButton { icon = "sports_esports", title = "Play", action = AddFriendRequested other }
    in
    div [class "profile-page container mx-auto px-4 mt-10 md:mt-28 md:max-w-5xl"]
            [ div [ class "relative flex flex-col min-w-0 break-words bg-white w-full mb-6 shadow-xl rounded-lg surface-1 on-surface-text"]
                [ div [ class "px-2 md:px-6"]
                    [ div [ class "flex flex-row md:flex-col justify-between" ]
                        [ div [ class "w-28 md:w-full px-0 md:px-4 flex justify-left md:justify-center"]
                            [ img
                                [ attribute "referrerpolicy" "no-referrer"
                                , src (Avatar.toString other.avatar)
                                , class "filter drop-shadow-sm sm:drop-shadow-xl rounded-lg align-middle border-none"
                                , class "absolute mt-2 md:-mt-16 md:w-40 w-20 md:h-40 h-20"
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
                                , class "py-0 sm:py-3 sm:pt-3"
                                ]
                                [ counter other.friendsCount "Friends"
                                , counter other.gamesCount "Games"
                                ]
                            ]
                        ]
                    , div [ class "text-center mt-3 sm:mt-12" ]
                        [ h3 [ class "text-xl sm:text-4xl font-semibold leading-normal sm:mb-14 text-slate-700 mb-3"] [ text (Name.toString other.name) ]
                        ]
                    ]
                ]
            , friendsList me other
            ]

friendsList me other =
    let
        friends = other.friends |> Maybe.withDefault []
    in
    if other.uid == (User.info me).uid && List.length friends > 0 then
        div
            [ class "relative flex flex-col min-w-0 break-words bg-white w-full mb-6 shadow-xl rounded-lg surface-1 on-surface-text"]
            [ div [class "tertiary-container on-tertiary-container-text rounded-t-lg py-2 px-4 font-bold"] [text "Friends" ]
            , div [] (List.map (\x -> friend me x ) friends)
            ]

    else
        text ""

friend : a -> User.Info -> Html msg
friend me other =
     div
        [ class "flex flex-row items-center"]
        [ a
            [ Route.href (other |> .uid |> Route.Profile)
            , class "h-12 w-12 m-2"
            ]
            [ img
                [ src (Avatar.toString other.avatar)
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
            [ button
                  [ class "primary on-primary-text text-white font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none ease-linear transition-all duration-150"
                  , class "px-3 py-2 m-1 mb-1"
                  , class "filter drop-shadow"
                  ]
                  [ span [ class "material-icons md-18" ][ text "sports_esports"]
                  ]
            ]
        ]


--actionButton : { a | icon : String, title : String, action :  } -> Html msg
actionButton : { a | icon : String, title : String, action : Msg } -> Html Msg
actionButton {icon, title, action} =
    button
        [ class "primary on-primary-text text-white font-bold inline-block flex items-center leading-normal uppercase text-xs rounded outline-none focus:outline-none ease-linear transition-all duration-150"
        , class "px-4 py-2 m-1 mb-1"
        , class "filter drop-shadow"
        , onClick action
        ]
        [ span [ class "material-icons md-18 mr-2" ][ text icon]
        , text title
        ]

counter : Int -> String -> Html msg
counter int label =
    div [ class "mr-0 sm:mr-4 px-3 sm:p-3 pt-3 pb-2 text-center"]
        [ span
            [ class "text-xl font-bold sm:block uppercase tracking-wide text-slate-600"
            , class "mr-1 sm:mr-0"
            ]
            [ text (String.fromInt int) ]
        , span [ class "text-sm text-slate-400" ]
            [ text label ]
        ]