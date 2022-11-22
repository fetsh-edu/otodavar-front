module Game exposing (..)

import Browser exposing (Document)
import Game.Game as Game exposing (Game)
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import OtoApi exposing (config)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Session exposing (Session)
import User.Bearer as Bearer
import User.Uid as Uid exposing (Uid)
import User.User exposing (User)

type alias Model =
    { session : Session
    , game : WebData Game
    }

type Msg
    = GameReceived (WebData Game)

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


decoder : Decoder Game
decoder = Game.decoder


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

gameView : User -> WebData Game -> List (Html msg)
gameView me game =
    let
        some =
            case game of
                NotAsked -> text "not asked"


                Loading -> text "loading"


                Failure e -> text "failure"


                Success a -> text "success"


    in
    [ some
    ]
