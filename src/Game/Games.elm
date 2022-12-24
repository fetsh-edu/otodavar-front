module Game.Games exposing (..)

import Game.OtoGame as Game exposing (OtoGame)
import Game.Page as Page exposing (Page)
import Json.Decode as Decode exposing (Decoder)
type alias Games =
    { randomGame : Maybe OtoGame
    , archivedGames : Page OtoGame
    , notSeenGames : List OtoGame
    , myTurnGames : List OtoGame
    , stalledGames : Page OtoGame
    }

decoder : Decoder Games
decoder =
    Decode.map5 Games
        (Decode.field "random_game" (Decode.maybe Game.decoder))
        (Decode.field "closed_games" (Page.decoder Game.decoder))
        (Decode.field "win_games" (Decode.list Game.decoder))
        (Decode.field "my_turn_games" (Decode.list Game.decoder))
        (Decode.field "stalled_games" (Page.decoder Game.decoder ))