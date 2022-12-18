module Home exposing (..)

import Api.OtoRequest as OtoRequest
import Browser exposing (Document)
import Game.OtoGame as Game exposing (OtoGame)
import Game.Games as Games exposing (Games)
import Game.Game as SGame exposing (Game(..))
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Api.OtoApi as OtoApi
import RemoteData exposing (RemoteData(..), WebData)
import Route
import SharedModel exposing (SharedModel)
import Url exposing (Url)
import User.Bearer exposing (Bearer)
import User.Handle exposing (Handle)
import User.Uid exposing (Uid)
import User.User as User exposing (SimpleInfo, User)
import View.Helper exposing (nbsp)

type alias Model =
    { session : SharedModel
    , home : WebData Games
    , stalled : WebData (List OtoGame)
    }

toSession : Model -> SharedModel
toSession model =
    model.session

initModel : SharedModel -> Model
initModel session = { session = session, home = NotAsked, stalled = NotAsked }

init : SharedModel -> ( Model, Cmd Msg )
init session =
    let
        model = initModel session
    in
    ( { model | home = Loading }, get (SharedModel.bearer session) (session.apiUrl) )

updateSession : SharedModel -> Model -> Model
updateSession session model =
    { model | session  = session }

type Msg
    = HomeReceived (WebData Games)
    | LoadStalled
    | StalledReceived (WebData (List OtoGame))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeReceived webData ->
            ( { model | home = webData }, Cmd.none )
        LoadStalled ->
            ( { model | stalled = Loading }, getStalled (SharedModel.bearer model.session) (model.session.apiUrl) )
        StalledReceived some ->
            let
                newModel = { model | stalled = some }
            in
            case some of
                Success games_ ->
                    ( { newModel | home = RemoteData.map (\games -> { games | stalledPreviewGames = games_ }) model.home }
                    , Cmd.none
                    )
                _ -> ( newModel, Cmd.none )


type alias Translator msg =
    { toSelf : Msg -> msg
    , onRandomLaunch : Maybe Uid -> msg
    }
view : Translator msg -> Model -> Document msg
view translator { session, home, stalled } =
    let
        body =
            case SharedModel.user session of
                Nothing -> [ View.Helper.loadingContainer "Please refresh this page. And if you can, tell about this error to developer." ]
                Just user ->
                    case home of
                        NotAsked -> [ View.Helper.loadingContainer "Not asked" ]
                        Loading -> loadingContent
                        Failure e -> [ View.Helper.loadingContainer "Failure" ]
                        Success a -> successContent translator user session a stalled

    in
        { title = ""
        , body = body
        }


loadingContent : List (Html msg)
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

successContent : Translator msg -> User -> SharedModel -> Games -> WebData (List OtoGame)-> List (Html msg)
successContent { onRandomLaunch, toSelf } me session games stalled =
    let
        mappedGames games_ = List.map (SGame.fromGame (me |> User.info |> .uid |> Just)) <| games_
    in
    [ View.Helper.container
        [ winSection  <| mappedGames games.notSeenGames
        , myTurnSection <| mappedGames games.myTurnGames
        , playButtonsSection me games.randomGame onRandomLaunch
        , partnersTurnSection toSelf stalled games.stalledCount games.totalStalledCount <| mappedGames games.stalledPreviewGames
        , oldGamesSection (games.archivedGames |> List.map (SGame.fromGame (me |> User.info |> .uid |> Just)))
        ]
    ]

myTurnSection : List SGame.Game -> Html msg
myTurnSection games =
    gamesSection "Your turn!" "tertiary-container on-tertiary-container-text" games

oldGamesSection : List SGame.Game -> Html msg
oldGamesSection games =
    gamesSection "Previous Games" "tertiary-container on-tertiary-container-text" games

partnersTurnSection : (Msg -> msg) -> WebData (List OtoGame)-> Int -> Int -> List SGame.Game -> Html msg
partnersTurnSection toMsg stalled stalledCount totalStalledCount games =
    let
        footer =
            if stalledCount < totalStalledCount
            then
                case stalled of
                    Success _ -> Nothing
                    _ ->
                        ( Just
                            (
                                [ class "uppercase text-center invisible-click surface-1 cursor-pointer select-none"
                                , case stalled of
                                    Loading -> class ""
                                    _ -> onClick (toMsg LoadStalled)
                                ]
                                , [ case stalled of
                                    Loading -> text "Loading"
                                    NotAsked -> text ("Show all (+" ++ String.fromInt(totalStalledCount - stalledCount) ++ ")" )
                                    Failure _ -> text "Error"
                                    Success _ -> text ""
                                  ]
                            )
                        )
            else Nothing

    in
    gamesSectionWithFooter
        "Waiting for partner"
        "secondary-container on-secondary-container-text"
        games
        footer

winSection : List SGame.Game -> Html msg
winSection games =
    gamesSection "Win!" "secondary-container on-secondary-container-text" games

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

playAFriendButton : { a | handle : Handle} -> Html msg
playAFriendButton me =
    div
        [ class "flex flex-row items-center "]
        [ a
            [ Route.href (me |> .handle |> Route.Profile)
            , class "h-12 w-12 m-2 invisible-click"
            ]
            [ span [ class "material-symbols-outlined md-48" ][ text "add_circle" ]
            ]
        , a
            [ Route.href (me |> .handle |> Route.Profile)
            , class "ml-2 flex-1 py-3 invisible-click"
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
            [ class "h-12 w-12 m-2 cursor-pointer invisible-click"
            , onClick (action Nothing)
            ]
            [ span [ class "material-symbols-outlined md-48" ][ text "auto_awesome" ]
            ]
        , span
            [ class "ml-2 flex-1 py-3  cursor-pointer invisible-click"
            , onClick (action Nothing)
            ]
            [ span [] [text "Play a Random Partner"]
            ]
        ]


gamesSection : String -> String -> List SGame.Game -> Html msg
gamesSection title classes games = gamesSectionWithFooter title classes games Nothing

gamesSectionWithFooter : String -> String -> List SGame.Game -> Maybe (List (Html.Attribute msg), List (Html msg)) -> Html msg
gamesSectionWithFooter title classes games footer =
    if List.isEmpty games
        then text ""
        else View.Helper.sectionWithFooter title (classes ++ " uppercase text-center") (List.map (SGame.gamePreview) games) footer

get : Maybe Bearer -> Url -> Cmd Msg
get bearer apiUrl =
    OtoRequest.get
        bearer
        ((OtoApi.routes apiUrl).home)
        HomeReceived
        Games.decoder

getStalled : Maybe Bearer -> Url -> Cmd Msg
getStalled bearer apiUrl =
    OtoRequest.get
        bearer
        ((OtoApi.routes apiUrl).game.stalled)
        StalledReceived
        (Decode.list Game.decoder)