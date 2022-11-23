module Game.Games exposing (..)

import Game.OtoGame as Game exposing (OtoGame)
import Json.Decode as Decode exposing (Decoder)
type alias Games =
    { randomGame : Maybe OtoGame
    , openGames : List OtoGame
    , closedGames : List OtoGame
    }

decoder : Decoder Games
decoder =
    Decode.map3 Games
        (Decode.field "random_game" (Decode.maybe Game.decoder))
        (Decode.field "open_games" (Decode.list Game.decoder))
        (Decode.field "closed_games" (Decode.list Game.decoder))
