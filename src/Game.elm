module Game exposing (..)

import Browser exposing (Document)
import Game.GameStatus as Status
import Game.OtoGame as OtoGame exposing (OtoGame)
import Game.Game as Game exposing (Game(..))
import Game.Round as Round exposing (Round(..))
import Game.Word as Word
import Helpers exposing (onEnter)
import Html exposing (Html, button, div, input, p, span, text)
import Html.Attributes exposing (class, disabled, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import OtoApi exposing (config)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Session exposing (Session)
import User.Avatar as Avatar
import User.Bearer as Bearer
import User.Name as Name
import User.Uid as Uid exposing (Uid)
import User.User as User exposing (User)
import View.Helper

type alias Model =
    { session : Session
    , game : WebData Game
    , guess : String
    , guessWebData : WebData Game
    }

type Msg
    = GameReceived (WebData Game)
    | OnGuessChange String
    | SubmitGuess
    | OnGuessResponse (WebData Game)

initModel : Session -> Model
initModel = Model >> (\x -> x Loading "" NotAsked)

toSession : Model -> Session
toSession = .session

init : Session -> Uid -> (Model, Cmd Msg)
init session uid =
    (initModel session, get session uid)

updateSession : Session -> Model -> Model
updateSession session model =
    { model | session  = session}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameReceived webData ->
            ( { model | game = webData }, Cmd.none)
        OnGuessChange str ->
            ( { model | guess = str }, Cmd.none)
        SubmitGuess ->
            case model.game of
                Success game ->
                    ( {model | guessWebData = Loading, guess = "" }, submitGuess model.session model.guess game)
                _ ->
                    (model, Cmd.none)
        OnGuessResponse ((Success s) as webData) ->
            ( { model | game = webData, guessWebData = NotAsked }, Cmd.none)
        OnGuessResponse webData ->
            ( { model | guessWebData = webData }, Cmd.none)

launchCmd : Session -> Maybe Uid -> Cmd Msg
launchCmd session maybeUid =
    let
        uidEncoder =
            maybeUid
                |> Maybe.map (Uid.encode >> (\x -> ("user_uid", x)) >> List.singleton >> Encode.object)
                |> Maybe.withDefault Encode.null
        url = OtoApi.routes.game.start
        message bearer =
            RemoteData.Http.postWithConfig
                (OtoApi.config bearer)
                url
                GameReceived
                (Game.decoder (session |> Session.user |> Maybe.map(.uid << User.info)))
                uidEncoder
    in
    session |> Session.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


get : Session -> Uid -> Cmd Msg
get session uid =
    let
        url = OtoApi.routes.game.show uid
        decoder = (Game.decoder (session |> Session.user |> Maybe.map(.uid << User.info)))
        message bearer = RemoteData.Http.getWithConfig (config bearer) url OnGuessResponse decoder
    in
    session |> Session.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


submitGuess : Session -> String -> Game -> Cmd Msg
submitGuess session guess game =
    case game of
        WrongState _ -> Cmd.none
        RightState state ->
            let
                url = OtoApi.routes.word.create
                decoder = (Game.decoder (session |> Session.user |> Maybe.map(.uid << User.info)))
                guessValue =
                    Word.encoder
                        (state |> Game.payload |> .uid)
                        (state |> Game.payload |> .question |> Maybe.map Round.id |> Maybe.map ((+) 1) |> Maybe.withDefault 0)
                        (guess)
                message bearer = RemoteData.Http.postWithConfig (config bearer) url OnGuessResponse decoder guessValue
            in
            session |> Session.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


type alias Translator msg =
    { toSelf : Msg -> msg
    }

view : Translator msg -> Model -> Document msg
view translator model =
    { title = "Game"
    , body =
        case model |> toSession |> Session.user of
            Nothing -> [ text "SHOULDN'T BE POSSIBLE" ]
            Just me -> gameView translator me model
    }

gameView : Translator msg -> User -> Model -> List (Html msg)
gameView translator me model =
    let
        some =
            case model.game of
                -- TODO: Handle this
                NotAsked -> [ View.Helper.smallContainer "not asked" ]
                Loading -> [ View.Helper.smallContainer "loading" ]
                Failure e -> [View.Helper.smallContainer "failure" ]
                Success a -> successContent translator me model.guess a
    in
    some

successContent : Translator msg -> User -> String -> Game -> List (Html msg)
successContent translator me value_ sGame =
    case sGame of
        WrongState otoGame -> [View.Helper.smallContainer "TODO:" ]
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
                Just user -> Avatar.img user.avatar "w-20 h-20  filter drop-shadow"
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

        speechBubble =
            case sGame of
                Game.Others _ _ _ -> text ""
                Game.Mine _ _ p ->
                    div
                        [ class "flex z-10 mt-2 surface-7 on-surface-text text-sm p-2 pl-3 w-full h-12 rounded-lg filter drop-shadow speech"
                        , speechClass
                        ] (bubbleContent p)

        speechClass =
            if (Game.payload sGame |> .status) == Status.Closed
            then class "speech-left speech-right"
            else class "speech-left"

        bubbleContent : Game.Payload -> List (Html msg)
        bubbleContent p =
            case p.status of
                Status.Closed ->
                    [text "TODO: "]
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
            case Game.payload sGame |> .question of
                Just a ->
                    roundView Big a
                Nothing ->
                    case sGame of
                        Game.Mine _ _ p_ ->
                            case p_.guess of
                                Game.LeftGuess _ -> p [ class "text-center mt-4"] [ text "Now it's partners turn" ]
                                _ -> p [ class "text-center mt-4"] [ text "Say your first word, any word!" ]
                        Game.Others _ _ _ -> text ""

    in
    div
        [ class "tertiary-container on-tertiary-container-text rounded-lg p-4 filter drop-shadow overflow-hidden"]
        [ div [class "flex z-10 flex-row justify-around"]
            [ span [ class "flex items-center w-40 flex-col truncate overflow-ellipses" ] [ Avatar.img leftUser.avatar "w-20 h-20  filter drop-shadow", span [] [text leftUserName] ]
            , span
                [ class "flex items-center w-40 flex-col z-1 relative" ]
                [ rightUserAvatar, span [class "pt-2 text-sm on-surface-variant-text truncate overflow-ellipses"] [text rightUserName]
                , readyBubble
                ]
            ]
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



-- TODO: ORDER
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