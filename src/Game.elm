port module Game exposing (..)

import Browser exposing (Document)
import Game.GameStatus as Status
import Game.OtoGame as OtoGame exposing (OtoGame)
import Game.Game as Game exposing (Game(..))
import Game.Round as Round exposing (Round(..))
import Game.Word as Word exposing (Word)
import Helpers exposing (onEnter)
import Html exposing (Html, a, button, div, input, p, span, text)
import Html.Attributes exposing (class, disabled, id, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import OtoApi exposing (config)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Route
import SharedModel exposing (SharedModel)
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
    }

type Msg
    = GameReceived (WebData Game)
    | OnGuessChange String
    | SubmitGuess
    | OnGuessResponse (WebData Game)
    | GotWordFromSocket (Result Decode.Error Word)

initModel : SharedModel -> Model
initModel = Model >> (\x -> x Loading "" NotAsked)

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
            ( { model | game = webData }
            , webData
                |> RemoteData.map (Game.uid >> joinChannelSocket)
                |> RemoteData.withDefault Cmd.none
            )
        OnGuessChange str ->
            ( { model | guess = str }, Cmd.none)
        SubmitGuess ->
            case (model.game, model.guess |> String.trim |> String.isEmpty) of
                (Success game, False) ->
                    ( {model | guessWebData = Loading, guess = "" }, submitGuess model.session model.guess game)
                _ ->
                    (model, Cmd.none)
        OnGuessResponse ((Success s) as webData) ->
            ( { model | game = webData, guessWebData = NotAsked }, Cmd.none)
        OnGuessResponse webData ->
            ( { model | guessWebData = webData }, Cmd.none)
        GotWordFromSocket (Ok word) ->
            if model |> toSession |> SharedModel.user |> Maybe.map User.info |> Maybe.map (\x -> x.uid /= word.player) |> Maybe.withDefault False
            then ( model, model.game |> RemoteData.map (Game.uid >> get (model.session)) |> RemoteData.withDefault Cmd.none )
            else ( model, Cmd.none )
        GotWordFromSocket (Err e) ->
            (model, Cmd.none)

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
                Success a -> successContent translator me model.guess a
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

successContent : Translator msg -> User -> String -> Game -> List (Html msg)
successContent translator me value_ sGame =
    case sGame of
        WrongState otoGame -> [View.Helper.loadingContainer "Game is in some wrong state. It shouldn't be possible. If you can, send this url to developer." ]
        RightState state ->
            [ View.Helper.container
                [ div
                    [ class "secondary-container on-secondary-container-text rounded-lg relative" ]
                    [ currentGuess translator value_ state
                    , oldGuesses state
                    ]
                ]
            ]


currentGuess : Translator msg -> String -> Game.State -> Html msg
currentGuess translator value_ sGame =
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
        winBubble_ =
            if (Game.payload sGame |> .status) == Status.Closed
            then
                div [ class "flex justify-center items-center"]
                    [ div [ class "warp h-32 top-0 text-md" ]
                        [ span
                            [ id "warp-label", class "warp__placeholder" ]
                            [ text "sababa" ]
                        , span
                            [ class "presentation" ]
                            [ span [ class "warp__0 px-2 rounded-lg surface" ] [text "s"]
                            , span [ class "warp__1 px-2 rounded-lg surface" ] [text "a"]
                            , span [ class "warp__2 px-2 rounded-lg surface" ] [text "b"]
                            , span [ class "warp__3 px-2 rounded-lg surface" ] [text "a"]
                            , span [ class "warp__4 px-2 rounded-lg surface" ] [text "b"]
                            , span [ class "warp__5 px-2 rounded-lg surface" ] [text "a"]
                            , span [ class "warp__6 px-2 rounded-lg surface" ] [text "!"]
                            ]
                        ]
                    ]
            else text ""

        speechBubble =
            case sGame of
                Game.Others _ _ _ -> text ""
                Game.Mine _ _ p ->
                    div [ class "px-3"]
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
                    case p.guess of
                        Game.LeftGuess w ->
                            [ div [ class "flex w-full justify-center items-center font-medium text-lg truncate overflow-ellipses"] [span [class "uppercase"] [text w.word]]]
                        _ ->
                            bubbleInput translator value_
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
                        roundView Big a
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
            [ span [ class "flex items-center w-40 flex-col truncate overflow-ellipses" ] [ Avatar.img leftUser.avatar "w-20 h-20  filter drop-shadow", span [] [text leftUserName] ]
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
        , onEnter (translator.toSelf SubmitGuess)
        , class "flex-grow border-0 shy bg-transparent"
        , style "appearance" "none"
        , placeholder "SAY IT HERE..."
        , value value_  ] []
    , button_
    ]

type Size = Small | Big



roundView : Size -> Round -> Html msg
roundView size round =
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
                    if String.length w > 14
                    then class "text-sm"
                    else class ""
            in
            div
                [ class "flex justify-center items-center mt-4"]
                [ span [ class "w-full order-first text-right pr-3 uppercase font-bold", wordSize w1.word] [ text w1.word ]
                , span [ class "w-full order-last pl-3 uppercase  font-bold", wordSize w2.word] [ text w2.word ]
                , span [ class textSize, class "text-center rounded-full py-1 px-3 primary on-primary-text"] [ text <| String.fromInt <| w1.roundId ]
                ]


oldGuesses : Game.State -> Html msg
oldGuesses state =
        state |> Game.payload |> .previousRounds |> List.map (roundView Small) |> div [ class "px-4 pb-4"]


joinChannelSocket : Uid -> Cmd msg
joinChannelSocket gameUid =
    subscribeToGame (Uid.encode gameUid)

port subscribeToGame : Decode.Value -> Cmd msg

onGameMessageDecoded : (Result Decode.Error Word -> msg) ->Sub msg
onGameMessageDecoded toMsg =
    onGameMessage (Decode.decodeValue Word.decoder >> toMsg)

port onGameMessage : (Encode.Value -> msg) -> Sub msg