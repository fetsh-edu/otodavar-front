module Home exposing (..)

import Browser exposing (Document)
import Game.OtoGame as Game
import Game.Games as Games exposing (Games)
import Game.Game as SGame exposing (Game(..))
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import OtoApi exposing (config)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Route
import SharedModel exposing (SharedModel)
import User.Bearer as Bearer
import User.Uid exposing (Uid)
import User.User as User exposing (SimpleInfo, User)
import View.Helper exposing (nbsp)

type alias Model =
    { session : SharedModel
    , home : WebData Games
    }

toSession : Model -> SharedModel
toSession model =
    model.session

initModel : SharedModel -> Model
initModel session = {session = session, home = NotAsked}

init : SharedModel -> ( Model, Cmd Msg )
init session =
    let
        model = initModel session
    in
    ( { model | home = Loading }, get session )

updateSession : SharedModel -> Model -> Model
updateSession session model =
    { model | session  = session }

type Msg =
    HomeReceived (WebData Games)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeReceived webData ->
            ( { model | home = webData }, Cmd.none )


type alias Translator msg =
    { toSelf : Msg -> msg
    , onRandomLaunch : Maybe Uid -> msg
    }
view : Translator msg -> Model -> Document msg
view translator { session, home } =
    let
        body =
            case SharedModel.user session of
                Nothing -> [ View.Helper.loadingContainer "Please refresh this page. And if you can, tell about this error to developer." ]
                Just user ->
                    case home of
                        NotAsked -> [ View.Helper.loadingContainer "Not asked" ]
                        Loading -> loadingContent
                        Failure e -> [ View.Helper.loadingContainer "Failure" ]
                        Success a -> successContent translator user session a

    in
        { title = ""
        , body = body
        }


loadingContent =
    let
        fakeSection header list =
            div
            [ class "relative flex flex-col min-w-0 break-words w-full mb-6 shadow-xl rounded-lg surface-1 on-surface-1-text"]
            [ div [ class "rounded-t-lg py-2 px-4 font-bold surface-1 on-surface-variant-text text-center uppercase"] [ text header ]
            , div [] list
            ]
        fakeButton title_ text_ =
            div
                [ class "flex flex-row items-center surface-3-text"]
                [ span [ class "h-12 w-12 m-2" ] [ span [ class "material-symbols-outlined md-48" ][ text title_ ] ]
                , span [ class "ml-2 flex-1 py-3"] [ span [] [text text_] ]
                ]
    in
    [ View.Helper.container
        [ div [ class "animate-pulse"]
            [ fakeSection nbsp [ fakeButton "" "" ]
            , fakeSection "Loading" [ fakeButton "add_circle" "Play a Friend", fakeButton "auto_awesome" "Play a Random Partner" ]
            , fakeSection nbsp [ fakeButton "" "", fakeButton "" "", fakeButton "" "", fakeButton "" "" ]
            ]
        ]
    ]

successContent : Translator msg -> User -> SharedModel -> Games -> List (Html msg)
successContent { onRandomLaunch, toSelf } me session games =
    [ View.Helper.container
        [ myTurnSection (games.openGames |> List.map (SGame.fromGame (me |> User.info |> .uid |> Just)) |> List.filter SGame.isMyTurn)
        , playButtonsSection me games.randomGame onRandomLaunch
        , partnersTurnSection (games.openGames |> List.map (SGame.fromGame (me |> User.info |> .uid |> Just)) |> List.filter SGame.isPartnersTurn)
        , oldGamesSection (games.closedGames |> List.map (SGame.fromGame (me |> User.info |> .uid |> Just)))
        ]
    ]

myTurnSection : List SGame.Game -> Html msg
myTurnSection games =
    gamesSection "Your turn!" "tertiary-container on-tertiary-container-text" games

oldGamesSection : List SGame.Game -> Html msg
oldGamesSection games =
    gamesSection "Finished Games" "tertiary-container on-tertiary-container-text" games


playButtonsSection : User -> Maybe Game.OtoGame -> (Maybe Uid -> msg) -> Html msg
playButtonsSection a mbGame action=
    let
        playAFriend = Just (playAFriendButton (User.info a))
        playRandom =
            case mbGame of
                Just _ -> Nothing
                Nothing -> Just (playARandomButton action)
        buttons = [playAFriend, playRandom] |> List.filterMap identity
    in
    View.Helper.section "Play a game" "error-container on-error-container-text  uppercase text-center"
        buttons

playAFriendButton : { a | uid : Uid} -> Html msg
playAFriendButton me =
    div
        [ class "flex flex-row items-center"]
        [ a
            [ Route.href (me |> .uid |> Route.Profile)
            , class "h-12 w-12 m-2"
            ]
            [ span [ class "material-symbols-outlined md-48" ][ text "add_circle" ]
            ]
        , a
            [ Route.href (me |> .uid |> Route.Profile)
            , class "ml-2 flex-1 py-3"
            ]
            [ span
                []
                [text "Play a Friend"]
            ]
        ]

playARandomButton : (Maybe Uid -> msg) -> Html msg
playARandomButton action =
    div
        [ class "flex flex-row items-center"]
        [ span
            [ class "h-12 w-12 m-2 cursor-pointer"
            , onClick (action Nothing)
            ]
            [ span [ class "material-symbols-outlined md-48" ][ text "auto_awesome" ]
            ]
        , span
            [ class "ml-2 flex-1 py-3  cursor-pointer"
            , onClick (action Nothing)
            ]
            [ span [] [text "Play a Random Partner"]
            ]
        ]

partnersTurnSection : List SGame.Game -> Html msg
partnersTurnSection games =
    gamesSection "Waiting for partner" "secondary-container on-secondary-container-text" games


gamesSection : String -> String -> List SGame.Game -> Html msg
gamesSection title classes games =
    if List.isEmpty games
    then text ""
    else View.Helper.section title (classes ++ " uppercase text-center") (List.map (SGame.view) games)


get : SharedModel -> Cmd Msg
get session =
    let
        url = (OtoApi.routes session.apiUrl).home
        message bearer = RemoteData.Http.getWithConfig (config bearer) url HomeReceived Games.decoder
    in
    session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none