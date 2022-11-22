module Game.Games exposing (..)

import Game.Game as Game exposing (Game)
import Json.Decode as Decode exposing (Decoder)
type alias Games =
    { randomGame : Maybe Game
    , openGames : List Game
    , closedGames : List Game
    }

decoder : Decoder Games
decoder =
    Decode.map3 Games
        (Decode.field "random_game" (Decode.maybe Game.decoder))
        (Decode.field "open_games" (Decode.list Game.decoder))
        (Decode.field "closed_games" (Decode.list Game.decoder))
