module Game.OtoGame exposing (..)

import Game.GameStatus as GameStatus exposing (GameStatus(..))
import Game.Word as Word exposing (Word)
import Json.Decode as Decode exposing (Decoder)
import User.Uid as Uid exposing (Uid)
import User.User as User exposing (SimpleInfo, User)

type alias OtoGame =
    { uid: Uid
    , status : GameStatus
    , player_1 : SimpleInfo
    , player_2 : Maybe SimpleInfo
    , words : List Word
    }

updateWord : Word -> OtoGame -> OtoGame
updateWord word game =
    { game | words = List.map (\x -> if x.id == word.id then word else x ) game.words }

players : OtoGame -> List SimpleInfo
players game =
    case game.player_2 of
        Nothing ->
            [game.player_1]
        Just partner ->
            [partner, game.player_1]

decoder : Decoder OtoGame
decoder =
    Decode.map5 OtoGame
        (Decode.field "uid" Uid.decoder)
        (Decode.field "status" GameStatus.decoder)
        (Decode.field "player_1" User.decoderInfo)
        (Decode.field "player_2" (Decode.maybe User.decoderInfo))
        (Decode.oneOf
            [ (Decode.field "last_words" (Decode.list Word.decoder))
            , (Decode.field "words" (Decode.list Word.decoder))
            ]
        )