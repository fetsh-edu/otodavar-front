port module Game exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Game.ComposedStatus exposing (ComposedStatus)
import Game.Game as Game exposing (Game(..), State)
import Game.Round as Round exposing (Round(..))
import Game.Stamp as Stamp exposing (Stamp)
import Game.Word as Word exposing (Word)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Api.OtoApi as OtoApi exposing (config)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Route
import SharedModel exposing (SharedModel)
import Task
import User.Bearer as Bearer
import User.Uid as Uid exposing (Uid)
import User.User as User exposing (User)
import View.Helper exposing (nbsp)

type alias Model =
    { session : SharedModel
    , game : WebData Game
    , guess : String
    , guessWebData : WebData Game
    , archiveWebData : WebData Game
    , stickerSelectShown : Maybe { wordId: Int }
    }

type Msg
    = GameReceived (WebData Game)
    | OnGuessChange String
    | SubmitGuess
    | OnGuessResponse (WebData Game)
    | GotWordFromSocket (Result Decode.Error Word)
    | SelectSticker Int
    | SendSticker Stamp
    | OnSendSticker (WebData Bool)
    | CloseStickers
    | ArchiveGame Uid
    | NoOp

initModel : SharedModel -> Model
initModel = Model >> (\x -> x Loading "" NotAsked NotAsked Nothing)

toSession : Model -> SharedModel
toSession = .session

init : (Msg -> msg) -> SharedModel -> Uid -> (Model, Cmd msg)
init toSelf session uid =
    (initModel session, get (toSelf << GameReceived) session uid)

updateSession : SharedModel -> Model -> Model
updateSession session model =
    { model | session  = session}


update : {toSelf : Msg -> msg, onGameArchived : msg} -> Msg -> Model -> ( Model, Cmd msg )
update { toSelf, onGameArchived } msg model =
    case msg of
        GameReceived webData ->
            let
                gameRoute = webData |> RemoteData.map (Game.uid >> Route.Game) |> RemoteData.toMaybe
                currentRoute = Route.fromUrl (model.session.currentUrl)
            in
            ( { model | game = webData }
            , Cmd.batch
                [ webData
                    |> RemoteData.map (Game.uid >> joinChannelSocket)
                    |> RemoteData.withDefault Cmd.none
                , gameRoute |> Maybe.andThen ( \gR -> currentRoute |> Maybe.map( \cR ->
                    if cR /= gR
                    then Navigation.pushUrl (model |> toSession |> .key) (Route.routeToString gR)
                    else Cmd.none
                ) ) |> Maybe.withDefault Cmd.none
                , focusOnInput toSelf
                ]
            )
        OnGuessChange str ->
            ( { model | guess = str }, Cmd.none)
        SubmitGuess ->
            case (model.game, model.guess |> String.trim |> String.isEmpty) of
                (Success game, False) ->
                    ( {model | guessWebData = Loading }, submitGuess (toSelf << OnGuessResponse) model.session model.guess game)
                _ ->
                    (model, Cmd.none)
        OnGuessResponse ((Success s) as webData) ->
            ( { model | game = webData, guessWebData = NotAsked, guess = "" }, focusOnInput toSelf)
        OnGuessResponse webData ->
            ( { model | guessWebData = webData}, focusOnInput toSelf)
        GotWordFromSocket (Ok word) ->
            if model |> toSession |> SharedModel.user |> Maybe.map User.info |> Maybe.map (\x -> x.uid /= word.player) |> Maybe.withDefault False
            then ( model, model.game |> RemoteData.map (Game.uid >> get (toSelf << GameReceived) (model.session)) |> RemoteData.withDefault Cmd.none )
            else ( { model | game = RemoteData.map (Game.updateWord word) model.game }, Cmd.none )
        GotWordFromSocket (Err e) ->
            (model, Cmd.none)
        SelectSticker wordId ->
            case model.stickerSelectShown of
                Nothing ->
                    ({ model | stickerSelectShown = Just { wordId = wordId } }, Cmd.none)
                Just _ ->
                    ({ model | stickerSelectShown = Nothing }, Cmd.none)
        SendSticker sticker ->
            ( { model | stickerSelectShown = Nothing }
            , sendSticker (toSelf << OnSendSticker) model.session sticker model.stickerSelectShown
            )
        OnSendSticker _ ->
            (model, Cmd.none)
        CloseStickers ->
            ({ model | stickerSelectShown = Nothing }, Cmd.none)
        NoOp -> (model, Cmd.none)
        ArchiveGame uid ->
            ({ model | archiveWebData = Loading}, archiveGame onGameArchived model.session uid)




focusOnInput : (Msg -> msg) -> Cmd msg
focusOnInput toMsg = Task.attempt (\_ -> toMsg NoOp) (Dom.focus "word-input")

launchCmd : SharedModel -> Maybe Uid -> Cmd Msg
launchCmd session maybeUid =
    let
        uidEncoder =
            maybeUid
                |> Maybe.map (Uid.encode >> (\x -> ("user_uid", x)) >> List.singleton >> Encode.object)
                |> Maybe.withDefault Encode.null
        url = (OtoApi.routes session.apiUrl).game.start
        message bearer =
            RemoteData.Http.postWithConfig
                (OtoApi.config bearer)
                url
                GameReceived
                (Game.decoder (session |> SharedModel.user |> Maybe.map(.uid << User.info)))
                uidEncoder
    in
    session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


sendSticker : (WebData Bool -> msg) -> SharedModel -> Stamp -> Maybe { a | wordId : Int } -> Cmd msg
sendSticker msg session stamp stickerSelectShown =
    case stickerSelectShown of
        Nothing -> Cmd.none
        Just { wordId } ->
            let
                url = (OtoApi.routes session.apiUrl).word.stamp(wordId)
                message bearer = RemoteData.Http.postWithConfig (config bearer) url msg (Decode.null False) (Encode.object [ ("stamp", Stamp.encode stamp)])
            in
            session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none

archiveGame : msg -> SharedModel -> Uid -> Cmd msg
archiveGame toMsg session uid =
    let
        url = (OtoApi.routes session.apiUrl).game.archive uid
        decoder = (Game.decoder (session |> SharedModel.user |> Maybe.map(.uid << User.info)))
        message bearer = RemoteData.Http.postWithConfig (config bearer) url (always toMsg) decoder Encode.null
    in
    session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none

get : (WebData Game -> msg) -> SharedModel -> Uid -> Cmd msg
get msg session uid =
    let
        url = (OtoApi.routes session.apiUrl).game.show uid
        decoder = (Game.decoder (session |> SharedModel.user |> Maybe.map(.uid << User.info)))
        message bearer = RemoteData.Http.getWithConfig (config bearer) url msg decoder
    in
    session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


submitGuess : (WebData Game -> msg) -> SharedModel -> String -> Game -> Cmd msg
submitGuess msg session guess game =
    case game of
        WrongState _ -> Cmd.none
        RightState state ->
            let
                url = (OtoApi.routes session.apiUrl).word.create
                decoder = (Game.decoder (session |> SharedModel.user |> Maybe.map(.uid << User.info)))
                guessValue =
                    Word.encoder
                        (state |> Game.payload |> .uid)
                        (state |> Game.payload |> .question |> Maybe.map Round.id |> Maybe.map ((+) 1) |> Maybe.withDefault 0)
                        (guess)
                message bearer = RemoteData.Http.postWithConfig (config bearer) url msg decoder guessValue
            in
            session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


type alias Translator msg =
    { toSelf : Msg -> msg
    , onGameStart : Maybe Uid -> msg
    }

view : Translator msg -> Model -> Document msg
view translator model =
    { title = "Game"
    , body =
        case model |> toSession |> SharedModel.user of
            Nothing -> [ text "SHOULDN'T BE POSSIBLE" ]
            Just me -> gameView translator me model
    }

gameView : Translator msg -> User -> Model -> List (Html msg)
gameView translator me model =
    let
        some =
            case model.game of
                -- TODO: Handle this
                NotAsked -> [ View.Helper.loadingContainer "not asked" ]
                Loading -> [ loadingContent ]
                Success game ->
                    case game of
                        WrongState _ ->
                            [View.Helper.loadingContainer "Game is in some wrong state. It shouldn't be possible. If you can, send this url to developer." ]
                        RightState state ->
                            successContent translator model.guess model.guessWebData model.archiveWebData state model.stickerSelectShown (Game.composedStatus game)
                Failure e ->
                    case e of
                        BadStatus 404 -> [ View.Helper.notFound ]
                        _ -> [View.Helper.simpleSmallContainer [text "ERROR"]]
    in
    some


loadingContent : Html msg
loadingContent =
    let
        leftAvatar =
            span
                [ class "flex items-center w-40 flex-col truncate overflow-ellipses" ]
                [ span
                    [ class "w-20 h-20 filter drop-shadow-sm sm:drop-shadow-xl rounded-lg align-middle border-none"
                    , class "surface-variant"
                    ]
                    []
                , span [ class "tertiary-container-text"] [text nbsp]
                ]
        speechBubble =
            div
                [ class "flex z-10 mt-2 surface-7 on-surface-text text-sm p-2 pl-3 w-full h-12 rounded-lg filter drop-shadow speech"
                , class "speech-left"
                ]
                [ div [ class "flex w-full justify-center items-center font-medium text-lg truncate overflow-ellipses on-surface-variant-text"] [span [class "uppercase"] [text "loading"]] ]
    in
    View.Helper.container
        [ div
            [ class "surface-1 rounded-lg animate-pulse" ]
            [ div
                [ class "surface-1 on-tertiary-container-text rounded-lg p-4 filter drop-shadow overflow-hidden"]
                [ div [class "flex z-10 flex-row justify-around"]
                    [ leftAvatar
                    , span
                        [ class "flex items-center w-40 flex-col z-1 relative" ]
                        [ leftAvatar]
                    ]
                , speechBubble
                ]
            , div [ class "px-4 pb-4"] [ text nbsp ]
            ]
        ]

successContent : Translator msg -> String -> WebData Game -> WebData Game -> State -> Maybe { wordId : Int } -> ComposedStatus -> List (Html msg)
successContent translator guessText guessData archiveData state stickerSelect composedStatus =
    let
        gameViewTranslator =
            { playAgainMsg = translator.onGameStart << Just
            , closeStickersMsg = translator.toSelf CloseStickers
            , createStampMsg = translator.toSelf << SelectSticker
            , stampSelectMsg = translator.toSelf << SendSticker
            , submitGuessMsg = translator.toSelf SubmitGuess
            , onGuessChangeMsg = translator.toSelf << OnGuessChange
            , archiveGameMsg = translator.toSelf << ArchiveGame
            }
    in
    [View.Helper.container (Game.gameView gameViewTranslator stickerSelect guessText guessData archiveData state True composedStatus)]

joinChannelSocket : Uid -> Cmd msg
joinChannelSocket gameUid =
    subscribeToGame (Uid.encode gameUid)

port subscribeToGame : Decode.Value -> Cmd msg

onGameMessageDecoded : (Result Decode.Error Word -> msg) ->Sub msg
onGameMessageDecoded toMsg =
    onGameMessage (Decode.decodeValue Word.decoder >> toMsg)

port onGameMessage : (Encode.Value -> msg) -> Sub msg