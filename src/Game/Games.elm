module Game.Games exposing (..)

import Game.OtoGame as Game exposing (OtoGame)
import Json.Decode as Decode exposing (Decoder)
type alias Games =
    { randomGame : Maybe OtoGame
    , archivedGames : List OtoGame
    , notSeenGames : List OtoGame
    , myTurnGames : List OtoGame
    , stalledPreviewGames : List OtoGame
    , stalledCount : Int
    , totalStalledCount : Int
    }

decoder : Decoder Games
decoder =
    Decode.map7 Games
        (Decode.field "random_game" (Decode.maybe Game.decoder))
        (Decode.field "closed_games" (Decode.list Game.decoder))
        (Decode.field "win_games" (Decode.list Game.decoder))
        (Decode.field "my_turn_games" (Decode.list Game.decoder))
        (Decode.field "stalled_preview" (Decode.list Game.decoder))
        (Decode.field "stalled_count" Decode.int)
        (Decode.field "total_stalled_count" Decode.int)