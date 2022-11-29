port module Main exposing (Model, init, main, update, view)

import Browser exposing (Document, application)
import Browser.Navigation as Navigation exposing (Key)
import Game
import Game.Game as G
import Home
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Login exposing (convertBytes)
import Maybe
import Msg exposing (Msg(..))
import Notifications exposing (Notification, Notifications, onNotification)
import OAuth.Implicit as OAuth exposing (AuthorizationResultWith(..))
import OtoApi
import Profile
import RemoteData exposing (WebData)
import Route exposing (Route)
import SharedModel exposing (SharedModel, logout)
import Url exposing (Protocol(..), Url)
import User.User exposing (User(..))

main : Program Flags Model Msg
main =
    application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , view = view
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        defSub =
            Sub.batch
                [ randomBytes (GotLoginMsg << Login.GotRandomBytes)
                , SharedModel.changes AuthEmerged (getSharedModel model)
                , onNotification (GotNotificationsMsg << Notifications.GotNotification << Decode.decodeValue Notifications.decoder)
                , Game.onGameMessageDecoded (GotGameMsg << Game.GotWordFromSocket)
                ]
    in
    defSub
    --case model of
    --    Home _ -> defSub
    --    Login _ -> defSub
    --    Profile _ -> defSub

-- ---------------------------
-- MODEL
-- ---------------------------

type Model
    = Home Home.Model
    | Login Login.Model
    | Profile Profile.Model
    | Game Game.Model


type alias Flags =
  { bytes : Maybe (List Int)
  , bearer : Encode.Value
  , apiUrl : String
  }

init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        tokenResult = Login.parseToken url
        maybeBytes = Maybe.map convertBytes flags.bytes
        apiUrl = Url.fromString flags.apiUrl |> Maybe.withDefault OtoApi.defaultApiUrl
        sharedModel url_ = SharedModel.decode (SharedModel.guest navKey url_ apiUrl) flags.bearer
    in
    case (tokenResult, maybeBytes) of
        (OAuth.Error error, _) ->
            sharedModel url
                |> Login.initModel
                |> Login.update (Login.GotInitAuthError (Login.ErrAuthorization error))
                |> updateWith Login GotLoginMsg
        (OAuth.Success _, Nothing) ->
            sharedModel url
                |> Login.initModel
                |> Login.update (Login.GotInitAuthError Login.ErrStateMismatch)
                |> updateWith Login GotLoginMsg
        (OAuth.Success { accessToken, state, idToken } , Just bytes) ->
            let
                (state_, redirectUrl) =
                    case Maybe.map (String.split ";") state of
                        Just [a_, b_] -> (Just a_, b_ |> String.trim |> Url.fromString |> Maybe.withDefault url)
                        Just [a_] -> (Just a_, url)
                        _ -> (Nothing, url)
            in
            if state_ /= Just bytes.state || idToken.parsed.nonce /= bytes.state then
                sharedModel url
                    |> Login.initModel
                    |> Login.update (Login.GotInitAuthError Login.ErrStateMismatch)
                    |> updateWith Login GotLoginMsg
            else
                sharedModel redirectUrl
                    |> Login.initModel
                    |> Login.update (Login.GotGoogleToken idToken)
                    |> updateWith Login GotLoginMsg
        (OAuth.Empty, _) ->
            changeRouteTo
                (Route.fromUrl url)
                (Home (Home.initModel (sharedModel url)))
                |> ( \(newModel, newCmd) ->
                        ( newModel
                        , Cmd.batch
                            [ newCmd
                            , Notifications.get apiUrl (GotNotificationsMsg << Notifications.GotNotifications) (SharedModel.bearer (sharedModel url))
                            ]
                        )
                   )


getSharedModel : Model -> SharedModel
getSharedModel page =
    case page of
        Home session -> Home.toSession session
        Login model -> Login.toSession model
        Profile profile -> Profile.toSession profile
        Game game -> Game.toSession game


toRoute : Model -> Maybe Route
toRoute model =
    case model of
        Home _ -> Just Route.Home
        Login _ -> Just Route.Login
        Profile some -> Just (Route.Profile some.uid)
        Game some -> some.game |> RemoteData.map (G.uid >> Route.Game >> Just) |> RemoteData.withDefault Nothing


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            getSharedModel model
        protected =
            Maybe.withDefault True <| Maybe.map Route.isProtected maybeRoute
    in
    if (SharedModel.isGuest session) && protected then
        ( model, Route.replaceUrl (SharedModel.navKey session) Route.Login )
    else
        case maybeRoute of
            Nothing ->
                ( model, Route.replaceUrl (SharedModel.navKey session) Route.Home )
            Just Route.Logout ->
                (model, SharedModel.logout)
            Just Route.Login ->
                updateWith Login GotLoginMsg (Login.init session)
            Just Route.Home ->
                updateWith Home GotHomeMsg (Home.init session)
            Just (Route.Profile uid) ->
                updateWith Profile GotProfileMsg (Profile.init session uid)
            Just (Route.Game uid) ->
                let
                    newState = updateWith Game GotGameMsg (Game.init session uid)
                    oldState = (model, Cmd.none)
                in
                case model of
                    Game { game } ->
                        if game |> RemoteData.map (G.uid >> (/=) uid) |> RemoteData.withDefault False then
                            newState
                        else
                            oldState
                    _ -> newState



-- ---------------------------
-- PORTS
-- ---------------------------

port randomBytes : (List Int -> msg) -> Sub msg

-- ---------------------------
-- UPDATE
-- ---------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model) of
        (NoOp, _) ->
            noOp model
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl (SharedModel.navKey (getSharedModel model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( AuthEmerged sharedModel, _ ) ->
            let
                initUrl = model |> getSharedModel |> SharedModel.url |> Login.cleanUrl |> Url.toString
                newModel = Login <| Login.initModel sharedModel
            in
            (newModel, Cmd.batch
                [ Navigation.replaceUrl (SharedModel.navKey sharedModel) initUrl
                , Notifications.get (sharedModel.apiUrl) (GotNotificationsMsg << Notifications.GotNotifications) (SharedModel.bearer sharedModel)
                ]
            )

        ( GotLoginMsg subMsg, Login login_  ) ->
            updateWith Login GotLoginMsg (Login.update subMsg login_)
        ( GotLoginMsg _, _ ) -> noOp model


        ( GotProfileMsg subMsg, Profile login_  ) ->
            updateWith Profile GotProfileMsg (Profile.update subMsg login_)
        ( GotProfileMsg _, _) -> noOp model


        ( GotNotificationsMsg (Notifications.GotNotifications data), any ) ->
            noOp (updateNotifications any data)
        ( GotNotificationsMsg (Notifications.GotNotification n), any) ->
            noOp (appendNotifications any n)


        (HideNotifications, _) ->
            toggleNotifications model False
        (ShowNotifications, _) ->
            toggleNotifications model True

        ( UserInfoReceived userInfo, _ ) ->
            let
                newShared = model |> getSharedModel |> SharedModel.updateUserInfo userInfo
            in
            (model |> updateSharedModel newShared, Cmd.none)

        ( GotHomeMsg subMsg, Home subModel  ) ->
            updateWith Home GotHomeMsg (Home.update subMsg subModel)
        ( GotHomeMsg _, _ ) -> noOp model

        ( LaunchGame maybeUid, _) ->
            let
                session = model |> getSharedModel
            in
            (session |> Game.initModel |> Game, Cmd.map GotGameMsg (Game.launchCmd session maybeUid))
        ( GotGameMsg subMsg, Game subModel ) ->
            updateWith Game GotGameMsg (Game.update subMsg subModel)
        ( GotGameMsg _, _) -> noOp model

toggleNotifications : Model -> Bool -> (Model, Cmd Msg)
toggleNotifications model bool =
    let
        cmd =
            if bool then
                Cmd.none
            else
                model
                    |> getSharedModel |> SharedModel.notifications
                    |> Maybe.map (.items >> RemoteData.withDefault [] >> List.filter (not << .seen))
                    |> Maybe.andThen (List.head >> Maybe.map .id)
                    |> Maybe.map (Notifications.markAsSeen (model |> getSharedModel |> .apiUrl) (GotNotificationsMsg << Notifications.GotNotifications) (model |> getSharedModel |> SharedModel.bearer))
                    |> Maybe.withDefault Cmd.none

        newSession = model |> getSharedModel |> SharedModel.updateNotifications (\n -> {n | shown = bool})

        newModel = model |> updateSharedModel newSession
    in
    (newModel, cmd)

updateNotifications : Model -> (WebData (List Notification)) -> Model
updateNotifications model data =
    (model |> getSharedModel |> SharedModel.setNotifications data |> updateSharedModel) model

appendNotifications : Model -> (Result Decode.Error Notification) -> Model
appendNotifications model data =
    case data of
        Ok value ->
            ( model |> getSharedModel
            |> SharedModel.updateNotifications (\n -> { n | items = RemoteData.map (\i -> value :: i) n.items })
            |> updateSharedModel
            ) model
        Err error ->
            model


updateSharedModel : SharedModel -> Model -> Model
updateSharedModel session_ model =
    case model of
        Home subModel ->  subModel |> Home.updateSession session_ |> Home
        Login subModel ->  subModel |> Login.updateSession session_ |> Login
        Profile subModel ->  subModel |> Profile.updateSession session_ |> Profile
        Game subModel ->  subModel |> Game.updateSession session_ |> Game


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )

-- ---------------------------
-- VIEW
-- ---------------------------

view : Model -> Document Msg
view model =
    let
        mapOver : Document Msg -> Document Msg
        mapOver subView =
            let
                { title, body } = subView
            in
            { title = "oto|davar " ++ title
            , body =
                [ main_ [ class "surface on-surface-text pt-4 md:pt-10"
                        , style "height" "100%"
                        , style "color-scheme" "dark"
                        ]
                        [ header_ model
                        , div [] body
                        , footer_ (model |> getSharedModel |> SharedModel.user)
                        ]
                ]
            }
    in
    case model of
        Home subModel ->    Home.view {toSelf = GotHomeMsg, onRandomLaunch = LaunchGame} subModel |> mapOver
        Login subModel ->   Login.view { toSelf = GotLoginMsg } subModel |> mapOver
        Profile subModel -> Profile.view  { toSelf = GotProfileMsg, onGameStart = LaunchGame } subModel |> mapOver
        Game subModel ->    Game.view { toSelf = GotGameMsg } subModel |> mapOver



header_ : Model -> Html Msg
header_ model =
    let
        notificationPillVisibility =
            model
                |> getSharedModel
                |> SharedModel.notifications
                |> Maybe.map
                    ( .items
                    >> RemoteData.withDefault []
                    >> List.any (.seen >> not)
                    )
                |> Maybe.withDefault False
        notificationPill =
            if notificationPillVisibility then
                span
                    [ class "flex absolute h-3 w-3 top-0 right-0 -mt-1 -mr-1"]
                    [ span
                        [ class "animate-ping absolute inline-flex h-full w-full rounded-full bg-purple-400 opacity-75" ]
                        []
                    , span
                        [ class "relative inline-flex rounded-full h-3 w-3 bg-purple-500" ]
                        []
                    ]
            else
                text ""
    in
    header [class "flex flex-col container md:max-w-5xl relative"]
        [ span
            [ class "surface-1 on-surface-variant-text letter absolute top-0 left-0 ml-4  filter drop-shadow" ]
            [ span [ class "material-symbols-outlined md-18" ] [ text "notifications" ] ]
        , span
            [ class "cursor-pointer surface-1 on-surface-variant-text letter absolute top-0 right-0 mr-4 filter drop-shadow"
            , onClick ShowNotifications]
            [ span [ class "material-symbols-outlined md-18" ] [ text "notifications" ]
            , notificationPill
            ]
        , div
            [ class "flex flex-row justify-center" ]
            [ a
                [ Route.href Route.Home ]
                [ span [ class "tertiary-container on-tertiary-container-text letter"] [ text "o" ]
                , span [ class "-ml-1.5 tertiary-container on-tertiary-container-text  letter"] [ text "t" ]
                , span [ class "-ml-1.5 tertiary-container on-tertiary-container-text  letter"] [ text "o" ]
                , span [ class "secondary-container on-secondary-container-text letter"] [ text "d" ]
                , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "a" ]
                , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "v" ]
                , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "a" ]
                , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "r" ]
                ]
            ]
        , div [ class "secondary-text text-sm md:text-base pt-2 justify-center"] [ text "The game you've been waiting for so long"]
        , modal model
        ]

modal : Model -> Html Msg
modal model =
    case model |> getSharedModel |> SharedModel.notifications of
        Nothing -> text ""
        Just notifications ->
            let
                visibility =
                    if notifications.shown then
                        ""
                    else
                        "hidden"
                notifications_ =
                    case notifications.items of
                        RemoteData.NotAsked -> [ text "Not Asked" ]
                        RemoteData.Loading -> [ text "Loading" ]
                        RemoteData.Failure e -> [text "Error"]
                        RemoteData.Success a -> a |> List.map ( Notifications.view { onExit = HideNotifications} )


            in
            div
                [ class "fixed inset-0 bg-gray-600 bg-opacity-50 overflow-y-auto h-full w-full z-50 py-4 px-4"
                , class visibility
                , onClick HideNotifications
                ]
                [ div
                    [ class "notifications-container mx-auto border w-full md:max-w-md shadow-lg rounded-md surface on-surface-text h-full flex flex-col"
                    , onClickStopPropagation NoOp
                    ]
                    [ h3
                        [ class "border-b border-light-200 text-center pt-3 pb-3 flex-none text-lg leading-6 font-medium relative uppercase font-bold"]
                        [ text "Notifications"
                        , span [ onClick HideNotifications, class "material-symbols-outlined md-24 absolute right-0 pr-4 cursor-pointer" ] [ text "close" ]
                        ]
                    , div
                        [ class "overflow-y-auto flex-grow divide-y divide-light" ]
                        notifications_
                    ]
                ]


onClickStopPropagation : msg -> Html.Attribute msg
onClickStopPropagation msg =
    Html.Events.stopPropagationOn "click" <| Decode.succeed ( msg, True )

footer_ : Maybe User -> Html msg
footer_ user =
    case user of
        Nothing -> text ""
        Just user_ ->
            div [ class "flex flex-col m-10 justify-center items-center"]
                [ a [ Route.href Route.Logout ] [ text "Sign out" ]]
