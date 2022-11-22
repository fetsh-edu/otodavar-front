module Game.Game exposing (..)

import Game.GameStatus as GameStatus exposing (GameStatus(..))
import Game.Word as Word exposing (Word)
import Json.Decode as Decode exposing (Decoder)
import User.Uid as Uid exposing (Uid)
import User.User as User exposing (SimpleInfo, User)

type alias Game =
    { uid: Uid
    , status : GameStatus
    , player_1 : SimpleInfo
    , player_2 : Maybe SimpleInfo
    , words : List Word
    }

type Round
    = Complete Word Word
    | Incomplete Word

toRound : (Word, List Word) -> Round
toRound (word, words) =
    case List.head words of
        Just a -> Complete word a
        Nothing -> Incomplete word

rounds : List Word -> List (Word, List Word)
rounds = groupWhile ( \a b -> a.roundId == b.roundId )

players : Game -> List SimpleInfo
players game =
    case game.player_2 of
        Nothing ->
            [game.player_1]
        Just partner ->
            [partner, game.player_1]

decoder : Decoder Game
decoder =
    Decode.map5 Game
        (Decode.field "uid" Uid.decoder)
        (Decode.field "status" GameStatus.decoder)
        (Decode.field "player_1" User.decoderInfo)
        (Decode.field "player_2" (Decode.maybe User.decoderInfo))
        (Decode.oneOf
            [ (Decode.field "last_words" (Decode.list Word.decoder))
            , (Decode.field "words" (Decode.list Word.decoder))
            ]
        )



groupWhile : (a -> a -> Bool) -> List a -> List ( a, List a )
groupWhile isSameGroup items =
    List.foldr
        (\x acc ->
            case acc of
                [] ->
                    [ ( x, [] ) ]

                ( y, restOfGroup ) :: groups ->
                    if isSameGroup x y then
                        ( x, y :: restOfGroup ) :: groups

                    else
                        ( x, [] ) :: acc
        )
        []
        items