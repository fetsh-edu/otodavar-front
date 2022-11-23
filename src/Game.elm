module Game exposing (..)

import Browser exposing (Document)
import Game.GameStatus as Status
import Game.OtoGame as OtoGame exposing (OtoGame)
import Game.Game as Game exposing (Game(..))
import Game.Round exposing (Round(..))
import Html exposing (Html, button, div, input, p, span, text)
import Html.Attributes exposing (class, placeholder, style)
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
    , game : WebData OtoGame
    }

type Msg
    = GameReceived (WebData OtoGame)

initModel : Session -> Model
initModel = Model >> (\x -> x Loading)

toSession : Model -> Session
toSession = .session

init : Session -> Uid -> (Model, Cmd Msg)
init session uid =
    ({session = session, game = Loading}, get session uid)

updateSession : Session -> Model -> Model
updateSession session model =
    { model | session  = session}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameReceived webData ->
            ( { model | game = webData }, Cmd.none)


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
                decoder
                uidEncoder
    in
    session |> Session.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


get : Session -> Uid -> Cmd Msg
get session uid =
    let
        url = OtoApi.routes.game.show uid
        message bearer = RemoteData.Http.getWithConfig (config bearer) url GameReceived decoder
    in
    session |> Session.bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


decoder : Decoder OtoGame
decoder = OtoGame.decoder


type alias Translator msg =
    { toSelf : Msg -> msg
    }

view : Translator msg -> Model -> Document msg
view translator model =
    { title = "Game"
    , body =
        case model |> toSession |> Session.user of
            Nothing -> [ text "SHOULDN'T BE POSSIBLE" ]
            Just me -> gameView me model.game
    }

gameView : User -> WebData OtoGame -> List (Html msg)
gameView me game =
    let
        some =
            case game of
                -- TODO: Handle this
                NotAsked -> [ View.Helper.smallContainer "not asked" ]
                Loading -> [ View.Helper.smallContainer "loading" ]
                Failure e -> [View.Helper.smallContainer "failure" ]
                Success a -> successContent me a
    in
    some

successContent : User -> OtoGame -> List (Html msg)
successContent me game =
    let
        sGame = Game.fromGame (User.info me |> .uid) game
    in
    case sGame of
        WrongState otoGame -> [View.Helper.smallContainer "TODO:" ]
        RightState state ->
            [ View.Helper.container
                [ div
                    [ class "secondary-container on-secondary-container-text rounded-lg relative" ]
                    [ currentGuess state
                    , oldGuesses state
                    ]
                ]
            ]


currentGuess : Game.State -> Html msg
currentGuess sGame =
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
                        [ class "flex z-10 mt-2 surface-7 on-surface-text text-sm p-2 pl-3 w-full rounded-lg filter drop-shadow speech"
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
                            [ div [ class "flex w-full justify-center font-medium text-lg truncate overflow-ellipses"] [span [class "uppercase"] [text w.word]]]
                        _ ->
                            bubbleInput

        question =
            case Game.payload sGame |> .question of
                Just a ->
                    roundView Big a
                Nothing ->
                    case sGame of
                        Game.Mine _ _ p_ ->
                            case p_.guess of
                                Game.LeftGuess _ -> p [ class "text-center mt-4"] [ text "Now we are waiting for your partner's word" ]
                                _ -> p [ class "text-center mt-4"] [ text "Say your first word, any word!" ]
                        Game.Others _ _ _ -> text ""

    in
    div
        [ class "tertiary-container on-tertiary-container-text rounded-lg p-4 filter drop-shadow overflow-hidden"]
        [ div [class "flex z-10 flex-row justify-around"]
            [ span [ class "flex items-center w-40 flex-col truncate overflow-ellipses" ] [ Avatar.img leftUser.avatar "w-20 h-20  filter drop-shadow", span [] [text leftUserName] ]
            , span [ class "flex items-center w-40 flex-col truncate overflow-ellipses" ] [ rightUserAvatar, span [class "pt-2 text-sm on-surface-variant-text"] [text rightUserName] ]
            ]
        , glow
        , speechBubble
        , question
        ]

bubbleInput : List (Html msg)
bubbleInput =
    [ input [ class "flex-grow border-0 shy bg-transparent", style "appearance" "none", placeholder "SAY IT HERE..." ] []
    , button [ class "flex-none primary on-primary-text rounded-lg px-4 py-1 font-bold mr-0 ml-1" ] [ text "Say It!" ]
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
            div
                [ class "flex justify-center items-center mt-4"]
                [ span [ class "w-full order-first text-right pr-3 uppercase font-bold"] [ text w1.word ]
                , span [ class "w-full order-last pl-3 uppercase  font-bold"] [ text w2.word ]
                , span [ class textSize, class "text-center rounded-full py-1 px-3 primary on-primary-text"] [ text <| String.fromInt <| w1.roundId ]
                ]


oldGuesses : Game.State -> Html msg
oldGuesses state =
        state |> Game.payload |> .previousRounds |> List.map (roundView Small) |> div [ class "px-4 pb-4"]