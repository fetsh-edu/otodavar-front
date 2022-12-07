port module Game exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Game.GameStatus as Status
import Game.Game as Game exposing (Game(..))
import Game.Round as Round exposing (Round(..))
import Game.Stamp as Stamp exposing (Stamp)
import Game.Word as Word exposing (Word)
import Helpers exposing (onEnter)
import Html exposing (Html, a, button, div, input, p, span, text)
import Html.Attributes exposing (autocomplete, autofocus, class, disabled, id, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import OtoApi exposing (config)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Route
import SharedModel exposing (SharedModel)
import Task
import User.Avatar as Avatar
import User.Bearer as Bearer
import User.Name as Name
import User.Uid as Uid exposing (Uid)
import User.User as User exposing (User)
import View.Helper exposing (nbsp)

type alias Model =
    { session : SharedModel
    , game : WebData Game
    , guess : String
    , guessWebData : WebData Game
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
    | NoOp

initModel : SharedModel -> Model
initModel = Model >> (\x -> x Loading "" NotAsked Nothing)

toSession : Model -> SharedModel
toSession = .session

init : SharedModel -> Uid -> (Model, Cmd Msg)
init session uid =
    (initModel session, get session uid)

updateSession : SharedModel -> Model -> Model
updateSession session model =
    { model | session  = session}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                , focusOnInput
                ]
            )
        OnGuessChange str ->
            ( { model | guess = str }, Cmd.none)
        SubmitGuess ->
            case (model.game, model.guess |> String.trim |> String.isEmpty) of
                (Success game, False) ->
                    ( {model | guessWebData = Loading }, submitGuess model.session model.guess game)
                _ ->
                    (model, Cmd.none)
        OnGuessResponse ((Success s) as webData) ->
            ( { model | game = webData, guessWebData = NotAsked, guess = "" }, focusOnInput)
        OnGuessResponse webData ->
            ( { model | guessWebData = webData}, focusOnInput)
        GotWordFromSocket (Ok word) ->
            if model |> toSession |> SharedModel.user |> Maybe.map User.info |> Maybe.map (\x -> x.uid /= word.player) |> Maybe.withDefault False
            then ( model, model.game |> RemoteData.map (Game.uid >> get (model.session)) |> RemoteData.withDefault Cmd.none )
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
            , sendSticker model.session sticker model.stickerSelectShown
            )
        OnSendSticker _ ->
            (model, Cmd.none)
        CloseStickers ->
            ({ model | stickerSelectShown = Nothing }, Cmd.none)
        NoOp -> (model, Cmd.none)


focusOnInput : Cmd Msg
focusOnInput = Task.attempt (\_ -> NoOp) (Dom.focus "word-input")

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


sendSticker : SharedModel -> Stamp -> Maybe { a | wordId : Int } -> Cmd Msg
sendSticker session stamp stickerSelectShown =
    case stickerSelectShown of
        Nothing -> Cmd.none
        Just { wordId } ->
            let
                url = (OtoApi.routes session.apiUrl).word.stamp(wordId)
                message bearer = RemoteData.Http.postWithConfig (config bearer) url OnSendSticker (Decode.null False) (Encode.object [ ("stamp", Stamp.encode stamp)])
            in
            session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


get : SharedModel -> Uid -> Cmd Msg
get session uid =
    let
        url = (OtoApi.routes session.apiUrl).game.show uid
        decoder = (Game.decoder (session |> SharedModel.user |> Maybe.map(.uid << User.info)))
        message bearer = RemoteData.Http.getWithConfig (config bearer) url GameReceived decoder
    in
    session |> SharedModel.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


submitGuess : SharedModel -> String -> Game -> Cmd Msg
submitGuess session guess game =
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
                message bearer = RemoteData.Http.postWithConfig (config bearer) url OnGuessResponse decoder guessValue
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
                Success game -> successContent translator me model.guess model.guessWebData game model.stickerSelectShown
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

successContent : Translator msg -> User -> String -> WebData Game -> Game -> Maybe {wordId : Int} -> List (Html msg)
successContent translator me guessText guessData sGame stickerSelect =
    case sGame of
        WrongState otoGame -> [View.Helper.loadingContainer "Game is in some wrong state. It shouldn't be possible. If you can, send this url to developer." ]
        RightState state ->
            let
                stickerOne sticker =
                    span
                        [ class "border border-light-200 border-dashed p-1 px-2 m-1 rounded-md cursor-pointer"
                        , onClick (translator.toSelf (SendSticker sticker))
                        ]
                        [ text (Stamp.toIcon sticker), text " ", text (Stamp.toString sticker) ]
                stickersSelect =
                    case stickerSelect of
                        Nothing -> text ""
                        Just { wordId } ->
                            div [ class "absolute top-2 w-full select-none" ]
                                [ div [class "surface on-surface-text rounded-lg p-4 flex flex-wrap mx-2 opacity-95 justify-center"]
                                    (List.map stickerOne Stamp.all)
                                , span
                                    [ onClick (translator.toSelf CloseStickers)
                                    , class "absolute top-0 right-2 p-2 material-symbols-outlined md-24 cursor-pointer"
                                    ]
                                    [ text "close" ]
                                ]
                playAgainAction =
                    case state |> Game.opponent of
                        Just opponent ->
                            onClick <| translator.onGameStart <| Just <| opponent.uid
                        Nothing ->
                            class ""
                playAgainButton =
                    case state |> Game.payload |> .status of
                        Status.Closed ->
                            div
                                [ class "sticky bottom-0 right-0 left-0 container w-full" ]
                                [ span
                                    [ class "justify-center flex w-full" ]
                                    [ span [ class "border-surface border-4 surface rounded m-1 mb-2"] [ button
                                        [ class "cursor-pointer font-bold inline-block flex items-center leading-normal uppercase text-md rounded outline-none focus:outline-none filter drop-shadow primary on-primary-text px-4 py-2 m-0"
                                        , playAgainAction
                                        ]
                                        [ span [ class "material-symbols-outlined text-md mr-2" ][ text "sports_esports" ]
                                        , text "Play again"
                                        ]
                                    ]]
                                ]
                        Status.Open -> text ""
            in
            [ View.Helper.container
                [ div
                    [ class "secondary-container on-secondary-container-text rounded-lg relative mb-2" ]
                    [ currentGuess translator guessText state guessData
                    , oldGuesses translator state
                    , stickersSelect
                    ]
                , playAgainButton
                ]
            ]


currentGuess : Translator msg -> String -> Game.State -> WebData Game -> Html msg
currentGuess translator guessText sGame guessData =
    let
        leftUser =
            case sGame of
                Game.Mine leftPlayer _ _ -> leftPlayer
                Game.Others leftPlayer _ _ -> leftPlayer

        rightUser =
            case sGame of
                Game.Mine _ maybeRightPlayer _ -> maybeRightPlayer
                Game.Others _ maybeRightPlayer _ -> maybeRightPlayer

        rightUserAvatar =
            case rightUser of
                Just user -> a [ Route.href (user.uid |> Route.Profile)] [Avatar.img user.avatar "w-20 h-20  filter drop-shadow"]
                Nothing -> span [ class "surface w-20 h-20 filter drop-shadow rounded-lg flex items-center justify-center"] [span [ class "material-symbols-outlined md-72" ][ text "psychology_alt" ]]

        rightUserName =
            case rightUser of
                Just user -> Name.toString user.name
                Nothing -> "Random user"

        leftUserName =
            case sGame of
                Game.Mine _ _ _ -> ""
                Game.Others l _ _ -> l.name |> Name.toString

        glow = if (Game.payload sGame |> .status) == Status.Closed
                then div [class "absolute win-glow h-96 w-full -top-10 left-0", style "z-index" "-1"][]
                else text ""

        winBubble =
            if (Game.payload sGame |> .status) == Status.Closed
            then
                div
                    [ class "flex justify-center items-center absolute w-full top-8", style "z-index" "-1"]
                    [ span
                        [ class "surface-2 on-surface-text rounded-full py-3 px-4 text-2xl tracking-tight font-bold"]
                        [ span [ class "material-symbols-outlined font-bold text-3xl" ] [ text "done_all"]]
                        ]
            else text ""

        speechBubble =
            case sGame of
                Game.Others _ _ _ -> text ""
                Game.Mine _ _ p ->
                    let
                        guessLoadingClass =
                            case guessData of
                                Loading -> class "animate-pulse"
                                Failure e -> class ""
                                _ -> class ""
                    in
                    div [ class "px-3", guessLoadingClass]
                        [ div
                                [ class "flex z-10 mt-2 surface-7 on-surface-text text-sm p-2 pl-3 w-full h-12 rounded-lg filter drop-shadow speech"
                                , speechClass
                                ] (bubbleContent p)
                        ]

        speechClass =
            if (Game.payload sGame |> .status) == Status.Closed
            then class "speech-left speech-right"
            else class "speech-left"

        bubbleContent : Game.Payload -> List (Html msg)
        bubbleContent p =
            case p.status of
                Status.Closed ->
                    case p.question of
                        Nothing -> [text ""]
                        Just (Incomplete w) -> [text ""]
                        Just (Complete w _) ->
                            [ div [ class "flex w-full justify-center items-center font-medium text-lg truncate overflow-ellipses"] [span [class "uppercase"] [text w.word]]]
                Status.Open ->
                    case (p.guess, guessData) of
                        (Game.LeftGuess w, _) ->
                            [ div [ class "flex w-full justify-center items-center font-medium text-lg truncate overflow-ellipses"] [span [class "uppercase"] [text w.word]]]
                        (_, Loading) ->
                            [ div [ class "flex w-full justify-center items-center font-medium text-lg truncate overflow-ellipses"] [span [class "uppercase"] [text guessText]]]
                        _ ->
                            bubbleInput translator guessText
        readyBubble =
            case (Game.payload sGame).guess of
                Game.RightGuess _ ->
                    span [ class "thought absolute px-3 py-1 surface text-sm surface-7 on-surface-text -left-4 top-0"] [ text "ready"]
                _ -> text ""

        question =
            if (Game.payload sGame).status == Status.Closed
            then text ""
            else
                case Game.payload sGame |> .question of
                    Just a ->
                        roundView translator Big a
                    Nothing ->
                        case sGame of
                            Game.Mine _ _ p_ ->
                                case p_.guess of
                                    Game.LeftGuess _ -> p [ class "text-center mt-4"] [ text "Now it's partner's turn" ]
                                    _ -> p [ class "text-center mt-4"] [ text "Say your first word, any word!" ]
                            Game.Others _ _ _ -> text ""

    in
    div
        [ class "tertiary-container on-tertiary-container-text rounded-lg py-4 filter drop-shadow overflow-hidden"]
        [ div [class "flex z-10 flex-row justify-around"]
            [ span
                [ class "flex items-center w-40 flex-col truncate overflow-ellipses" ]
                [ Avatar.img leftUser.avatar "w-20 h-20  filter drop-shadow"
                , span [] [text leftUserName]
                ]
            , span
                [ class "flex items-center w-40 flex-col z-1 relative" ]
                [ rightUserAvatar
                , if (Game.payload sGame).status == Status.Closed
                    then text (String.fromChar '\u{00A0}')
                    else span [class "pt-2 text-sm on-surface-variant-text truncate overflow-ellipses"] [text rightUserName]
                , readyBubble
                ]
            ]
        , winBubble
        , glow
        , speechBubble
        -- TODO : Add errors
        , div [] []
        , question
        ]

bubbleInput : Translator msg -> String -> List (Html msg)
bubbleInput translator value_ =
    let
        button_ =
            if  value_ |> String.trim |> String.isEmpty
            then text ""
            else
                button
                    [ class "flex-none primary-container on-primary-container-text rounded-lg px-4 py-1 font-bold mr-0 ml-1"
                    , onClick (translator.toSelf SubmitGuess)
                    , disabled (value_ |> String.trim |> String.isEmpty)
                    ]
                    [ text "Say It!" ]
    in
    [ input
        [ onInput (translator.toSelf << OnGuessChange)
        , autofocus True
        , id "word-input"
        , autocomplete False
        , onEnter (translator.toSelf SubmitGuess)
        , class "flex-grow border-0 shy bg-transparent"
        , style "appearance" "none"
        , placeholder "SAY IT HERE..."
        , value value_  ] []
    , button_
    ]

type Size = Small | Big



roundView : Translator msg -> Size -> Round -> Html msg
roundView translator size round =
    let
        textSize =
            case size of
                Small -> "text-sm"
                Big -> "font-semibold"
    in
    case round of
        Incomplete _ -> text ""
        Complete w1 w2 ->
            let
                wordSize w =
                    if String.length w > 13
                    then class "text-sm"
                    else class ""
                rightSticker =
                    case w2.stamp of
                        Stamp.Nothing -> text ""
                        _ ->
                            case size of
                                Big ->
                                    span
                                        [ class "absolute -top-4 left-6 transform rotate-3 text-sm surface on-surface-text px-2 py-0 pb-1 rounded-lg opacity-60 select-none" ]
                                        [ text (Stamp.toIcon w2.stamp)
                                        , text " "
                                        , text (Stamp.toString w2.stamp)
                                        ]
                                Small ->
                                    span
                                        [ class "absolute right-0 text-sm px-0 py-0 rounded-lg opacity-50" ]
                                        [ text (Stamp.toIcon w2.stamp)
                                        ]
                leftSticker =
                    case w1.stamp of
                        Stamp.Nothing -> text ""
                        _ ->
                            case size of
                                Big ->
                                    span
                                        [ class "absolute -top-4 right-6 transform -rotate-3 text-sm surface on-surface-text px-2 py-0  pb-1 rounded-lg opacity-60 select-none" ]
                                        [ text (Stamp.toIcon w1.stamp)
                                        , text " "
                                        , text (Stamp.toString w1.stamp)
                                        ]
                                Small ->
                                    span
                                        [ class "absolute left-0 text-sm  px-0 py-0 rounded-lg opacity-50" ]
                                        [ text (Stamp.toIcon w1.stamp)
                                        ]
                stickerClick =
                    case size of
                        Big -> onClick (translator.toSelf (SelectSticker w2.id))
                        Small -> class ""
                cursorPointer =
                    case size of
                        Big -> class "cursor-pointer"
                        Small -> class ""


            in
            div
                [ class "flex justify-center items-center mt-4 select-none"]
                [ span
                    [ class "w-full order-first text-right pr-3 relative", wordSize w1.word]
                    [ span
                        [ class "uppercase font-bold"
                        ]
                        [ text w1.word ]
                    , leftSticker
                    ]
                , span
                    [ class "w-full order-last pl-3 relative invisible-click"
                    , cursorPointer
                    , wordSize w2.word
                    , stickerClick
                    ]
                    [ span
                        [ class "uppercase font-bold"
                        ]
                        [ text w2.word ]
                    , rightSticker
                    ]
                , span [ class textSize, class "text-center rounded-full py-1 px-3 primary on-primary-text"] [ text <| String.fromInt <| w1.roundId ]
                ]


oldGuesses : Translator msg -> Game.State -> Html msg
oldGuesses translator state =
        state |> Game.payload |> .previousRounds |> List.map (roundView translator Small) |> div [ class "px-4 pb-4"]


joinChannelSocket : Uid -> Cmd msg
joinChannelSocket gameUid =
    subscribeToGame (Uid.encode gameUid)

port subscribeToGame : Decode.Value -> Cmd msg

onGameMessageDecoded : (Result Decode.Error Word -> msg) ->Sub msg
onGameMessageDecoded toMsg =
    onGameMessage (Decode.decodeValue Word.decoder >> toMsg)

port onGameMessage : (Encode.Value -> msg) -> Sub msg