module Game.Round exposing (..)

import Game.Word exposing (Word)
import Helpers exposing (find, groupWhile)
import User.Uid exposing (Uid)

type Round
    = Complete Word Word
    | Incomplete Word

id : Round -> Int
id round =
    case round of
        Complete w _ -> w.roundId
        Incomplete w -> w.roundId

isIncomplete : Round -> Bool
isIncomplete round =
    case round of
        Incomplete _ -> True
        _ -> False

getWord : Uid -> Round -> Maybe Word
getWord uid round =
    case round of
        Incomplete w -> if w.player == uid then Just w else Nothing
        Complete w1 w2 -> find (\x -> x.player == uid) [w1, w2]

fromWords : List Word -> List Round
fromWords =
    groupWhile ( \a b -> a.roundId == b.roundId )
        >> List.map toRound

toRound : (Word, List Word) -> Round
toRound (word, words) =
    List.head words
        |> Maybe.map (Complete word)
        |> Maybe.withDefault (Incomplete word)