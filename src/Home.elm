module Home exposing (..)

import Api.OtoRequest as OtoRequest
import Browser exposing (Document)
import Game.OtoGame as Game exposing (OtoGame)
import Game.Games as Games exposing (Games)
import Game.Game as SGame exposing (Game(..))
import Game.Page as Page exposing (Page)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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
    , stalled : WebData (Page OtoGame)
    , archived : WebData (Page OtoGame)
    }

toSession : Model -> SharedModel
toSession model =
    model.session

initModel : SharedModel -> Model
initModel session = { session = session, home = NotAsked, stalled = NotAsked, archived = NotAsked }

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
    | LoadStalled Int
    | LoadArchived Int
    | StalledReceived (WebData (Page OtoGame))
    | ArchivedReceived (WebData (Page OtoGame))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeReceived webData ->
            ( { model | home = webData }, Cmd.none )
        LoadStalled page ->
            ( { model | stalled = Loading }, getStalled (SharedModel.bearer model.session) (model.session.apiUrl) page )
        LoadArchived page ->
            ( { model | archived = Loading }, getArchived (SharedModel.bearer model.session) (model.session.apiUrl) page )
        StalledReceived some ->
            let
                newModel = { model | stalled = some }
            in
            case some of
                Success games_ ->
                    ( { newModel | home = RemoteData.map (\games -> { games | stalledGames = games_ }) model.home }
                    , Cmd.none
                    )
                _ -> ( newModel, Cmd.none )
        ArchivedReceived some ->
            let
                newModel = { model | archived = some }
            in
            case some of
                Success games_ ->
                    ( { newModel | home = RemoteData.map (\games -> { games | archivedGames = games_ }) model.home }
                    , Cmd.none
                    )
                _ -> ( newModel, Cmd.none )


type alias Translator msg =
    { toSelf : Msg -> msg
    , onRandomLaunch : Maybe Uid -> msg
    }
view : Translator msg -> Model -> Document msg
view translator { session, home, stalled, archived } =
    let
        body =
            case SharedModel.user session of
                Nothing -> [ View.Helper.loadingContainer "Please refresh this page. And if you can, tell about this error to developer." ]
                Just user ->
                    case home of
                        NotAsked -> [ View.Helper.loadingContainer "Not asked" ]
                        Loading -> loadingContent
                        Failure e -> [ View.Helper.loadingContainer "Failure" ]
                        Success a -> successContent translator user session a stalled archived

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

successContent : Translator msg -> User -> SharedModel -> Games -> WebData (Page OtoGame) -> WebData (Page OtoGame) -> List (Html msg)
successContent { onRandomLaunch, toSelf } me session games stalled archived =
    let
        otoGameToGame = SGame.fromGame (me |> User.info |> .uid |> Just)
        mappedGames games_ = List.map otoGameToGame <| games_
    in
    [ View.Helper.container
        [ winSection  <| mappedGames games.notSeenGames
        , myTurnSection <| mappedGames games.myTurnGames
        , playButtonsSection me games.randomGame onRandomLaunch
        , pagedGamesSection "Waiting for partner" "secondary-container on-secondary-container-text" toSelf LoadStalled stalled games.stalledGames otoGameToGame
        , pagedGamesSection "Previous Games" "tertiary-container on-tertiary-container-text" toSelf LoadArchived archived games.archivedGames otoGameToGame
        ]
    ]

myTurnSection : List SGame.Game -> Html msg
myTurnSection games =
    gamesSection "Your turn!" "tertiary-container on-tertiary-container-text" games

pagedGamesSection : String -> String -> (a -> msg) -> (Int -> a) -> RemoteData e b -> Page c -> (c -> Game) -> Html msg
pagedGamesSection title titleAttr toMsg loadPage newPage currentPage itemsToGame =
    let
        footer =
            ( [ class "uppercase text-center invisible-click surface-1 select-none flex"]
            , case newPage of
                Failure _ -> [ text "Error" ]
                _ ->
                    let
                        prevPage =
                            if currentPage.page > 1
                            then
                                div
                                    [ class "w-5/12 rounded-lg py-2 surface-1 cursor-pointer flex items-center justify-center"
                                    , case newPage of
                                      Loading -> class ""
                                      _ -> onClick (toMsg (loadPage (currentPage.page + -1)))
                                    ]
                                    [ span [ class "material-symbols-outlined mr-2" ][ text "arrow_back" ], text "Newer" ]
                            else div [ class "w-5/12"] [ text "" ]
                        pageCounter =
                            case newPage of
                                Loading -> View.Helper.spinner []
                                _ -> text (String.fromInt(currentPage.page) ++ "/" ++ String.fromInt(currentPage.totalPages))
                        nextPage =
                            if currentPage.page == currentPage.totalPages
                            then div [ class "w-5/12"] [ text "" ]
                            else
                                div
                                    [ class "w-5/12 rounded-lg py-2 surface-1 cursor-pointer flex items-center justify-center"
                                    , case newPage of
                                      Loading -> class ""
                                      _ -> onClick (toMsg (loadPage (currentPage.page + 1)))
                                    ]
                                    [ text "Older", span [ class "material-symbols-outlined ml-2" ][ text "arrow_forward" ] ]
                    in
                        [ prevPage
                        , div [ class "w-2/12 flex items-center justify-center" ] [ pageCounter ]
                        , nextPage
                        ]
            )

        maybeFooter =
            if currentPage.totalPages > 1
            then Just footer
            else Nothing

    in
    gamesSectionWithFooter
        title
        titleAttr
        (List.map itemsToGame currentPage.items)
        maybeFooter


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

getStalled : Maybe Bearer -> Url -> Int -> Cmd Msg
getStalled bearer apiUrl page =
    OtoRequest.get
        bearer
        ((OtoApi.routes apiUrl).game.stalled page)
        StalledReceived
        (Page.decoder Game.decoder)

getArchived : Maybe Bearer -> Url -> Int -> Cmd Msg
getArchived bearer apiUrl page =
    OtoRequest.get
        bearer
        ((OtoApi.routes apiUrl).game.archived page)
        ArchivedReceived
        (Page.decoder Game.decoder)