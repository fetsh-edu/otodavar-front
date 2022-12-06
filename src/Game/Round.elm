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


updateWord : Word -> Round -> Round
updateWord word round =
    case round of
        Incomplete w ->
            Incomplete (if w.id == word.id then word else w)
        Complete w1 w2 ->
            Complete
                (if w1.id == word.id then word else w1)
                (if w2.id == word.id then word else w2)



getWord : Uid -> Round -> Maybe Word
getWord uid round =
    case round of
        Incomplete w -> if w.player == uid then Just w else Nothing
        Complete w1 w2 -> find (\x -> x.player == uid) [w1, w2]

fromWords : Uid -> List Word -> List Round
fromWords uid =
    groupWhile ( \a b -> a.roundId == b.roundId )
        >> List.map (toRound uid)

toRound : Uid -> (Word, List Word) -> Round
toRound uid (word, words) =
    List.head words
        |> Maybe.map (balance uid word)
        |> Maybe.withDefault (Incomplete word)

balance : Uid -> Word -> Word -> Round
balance uid w1 w2 =
    if w1.player == uid
    then Complete w1 w2
    else Complete w2 w1