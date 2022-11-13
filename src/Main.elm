port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document, application)
import Browser.Navigation as Navigation exposing (Key)
import Home
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as Encode
import Login exposing (convertBytes)
import Maybe
import OAuth.Implicit as OAuth exposing (AuthorizationResultWith(..))
import Profile
import Route exposing (Route)
import Session exposing (Session, logout)
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
                , Session.changes (GotLoginMsg << Login.GotSession) (Session.navKey (toSession model)) (Session.url (toSession model))
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


type alias Flags =
  { bytes : Maybe (List Int)
  , bearer : Encode.Value
  }

init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        tokenResult = Login.parseToken url
        maybeBytes = Maybe.map convertBytes flags.bytes
        session url_ = Session.decode navKey url_ flags.bearer
    in
    case (tokenResult, maybeBytes) of
        (OAuth.Empty, _) ->
            changeRouteTo (Route.fromUrl url)
                (Home (Home.initModel (session url)))
        (OAuth.Error error, _) ->
            ( Login (Login.Model (session url) (Login.Erred (Login.ErrAuthorization error))), Cmd.none )
        (OAuth.Success _, Nothing) ->
            ( Login (Login.Model (session url) (Login.Erred Login.ErrStateMismatch)), Cmd.none )
        (OAuth.Success { accessToken, state, idToken } , Just bytes) ->
            let
                (state_, redirectUrl) =
                    case Maybe.map (String.split ";") state of
                        Just [a_, b_] -> (Just a_, b_ |> String.trim |> Url.fromString |> Maybe.withDefault url)
                        Just [a_] -> (Just a_, url)
                        _ -> (Nothing, url)
            in
            if state_ /= Just bytes.state || idToken.parsed.nonce /= bytes.state then
                ( Login (Login.Model (session url) (Login.Erred Login.ErrStateMismatch)), Cmd.none )
            else
                ( Login (Login.Model (session redirectUrl) Login.Processing), Cmd.map GotLoginMsg (Login.getOtoBearer idToken))


toSession : Model -> Session
toSession page =
    case page of
        Home session -> Home.toSession session
        Login model -> Login.toSession model
        Profile profile -> Profile.toSession profile


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
        protected =
            Maybe.withDefault True <| Maybe.map Route.isProtected maybeRoute
    in
    if (Session.isGuest session) && protected then
        ( model, Route.replaceUrl (Session.navKey session) Route.Login )
    else
        case (Debug.log "maybeRoute" maybeRoute) of
            Nothing ->
                ( model, Route.replaceUrl (Session.navKey session) Route.Home )
            Just Route.Logout ->
                (Login (Login.Model session Login.Idle), logout)
            Just Route.Login ->
                updateWith Login GotLoginMsg model (Login.init session)
            Just Route.Home ->
                updateWith Home GotHomeMsg model (Home.init session)
            Just (Route.Profile uid) ->
                updateWith Profile GotProfileMsg model (Profile.init session uid)


-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = NoOp
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotProfileMsg Profile.Msg



port randomBytes : (List Int -> msg) -> Sub msg

--
-- Update
--

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model) of
        (NoOp, _) ->
            noOp model
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotLoginMsg subMsg, Login login_  ) ->
            updateWith Login GotLoginMsg model (Login.update subMsg login_)

        ( GotProfileMsg subMsg, Profile login_  ) ->
            updateWith Profile GotProfileMsg model (Profile.update subMsg login_)
        ( a, b ) ->
            let
                c = Debug.log "Shouldn't be here: " (a, b)
            in
            noOp model

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
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
        viewPage toMsg config =
            let
                { title, body } =
                    viewHelper (Session.user (toSession model)) config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Home session ->
            viewPage GotHomeMsg { title = "Game", content = Home.view session }

        Login model_ ->
            viewPage GotLoginMsg (Login.view model_)

        Profile profile_ ->
            viewPage GotProfileMsg (Profile.view profile_)



viewBody : Html msg -> Html msg -> Html msg
viewBody content footer =
    main_ [class "surface on-surface-text pt-10", style "height" "100%", style "color-scheme" "dark"]
        [ header_
        , content
        , footer
        ]


header_ : Html msg
header_ =
    header [class "flex flex-col"]
        [ div [ class "flex flex-row justify-center" ] [ a [ Route.href Route.Home ]
            [ span [ class "tertiary-container on-tertiary-container-text letter"] [ text "o" ]
            , span [ class "-ml-1.5 tertiary-container on-tertiary-container-text  letter"] [ text "t" ]
            , span [ class "-ml-1.5 tertiary-container on-tertiary-container-text  letter"] [ text "o" ]
            , span [ class "secondary-container on-secondary-container-text letter"] [ text "d" ]
            , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "a" ]
            , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "v" ]
            , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "a" ]
            , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "r" ]
            , span
                [ class "-ml-1.5 background on-background-text letter border border-gray-100" ]
                [ span [ class "material-icons md-18" ] [ text "notifications" ] ]
            ]]
        , div [ class "text-s pt-2 justify-center"] [ text "The game you've been waiting for so long"]
        ]

viewFooter : Maybe User -> Html msg
viewFooter user =
    case user of
        Nothing -> text ""
        Just user_ ->
            div [ class "flex flex-col m-10 justify-center items-center"]
                [ a [ Route.href Route.Logout ] [ text "Sign out" ]]


viewHelper : Maybe User -> { title : String, content : Html msg } -> Document msg
viewHelper maybeViewer { title, content } =
    { title = "oto|davar " ++ title
    , body = [viewBody content (viewFooter maybeViewer)]
    }
