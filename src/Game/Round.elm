module Game.Round exposing (..)

import Game.Stamp as Stamp
import Game.Word as Word exposing (Word)
import Helpers exposing (groupWhile)
import Html
import Html.Attributes
import Html.Events
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



type Size = Small | Big

view : (Int -> msg) -> Size -> Round -> Html.Html msg
view chooseStampMsg size round =
    case round of
        Incomplete _ -> Html.text ""
        Complete w1 w2 ->
            Html.div
                [ Html.Attributes.class "flex justify-center items-center mt-4 select-none"]
                [

                -- LEFT WORD
                Html.span
                    [ Html.Attributes.class "w-full order-first text-right pr-3 relative"
                    , Word.shrinkClass w1.word
                    ]
                    [ Html.span
                        [ Html.Attributes.class "uppercase font-bold"
                        ]
                        [ Html.text w1.word ]
                    , case size of
                      Big -> Stamp.bigView [ Html.Attributes.class "absolute -top-4 right-6 transform -rotate-3" ] w1.stamp
                      Small -> Stamp.smallView [ Html.Attributes.class "absolute left-0"] w1.stamp
                    ]

                -- RIGHT WORD
                , Html.span
                    [ Html.Attributes.class "w-full order-last pl-3 relative invisible-click"
                    , Word.shrinkClass w2.word
                    , case size of
                      Big -> Html.Attributes.class "cursor-pointer"
                      Small -> Html.Attributes.class ""
                    , case size of
                      Big -> Html.Events.onClick (chooseStampMsg w2.id)
                      Small -> Html.Attributes.class ""
                    ]
                    [ Html.span
                        [ Html.Attributes.class "uppercase font-bold"
                        , case size of
                          Big -> Html.Attributes.class "border-0 border-b border-dashed border-on-tertiary-container"
                          Small -> Html.Attributes.class ""
                        ]
                        [ Html.text w2.word ]
                    , case size of
                      Big -> Stamp.bigView [ Html.Attributes.class "absolute -top-4 left-6 transform rotate-3" ] w2.stamp
                      Small -> Stamp.smallView [ Html.Attributes.class "absolute right-0"] w2.stamp
                    ]

                -- MIDDLE CIRCLE
                , Html.span
                    [ case size of
                      Small -> Html.Attributes.class "text-sm"
                      Big -> Html.Attributes.class "font-semibold"
                    , Html.Attributes.class "text-center rounded-full py-1 px-3 primary on-primary-text"
                    ]
                    [ Html.text <| String.fromInt <| w1.roundId
                    ]
                ]
