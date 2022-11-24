module Game.Game exposing (..)

import Game.OtoGame as Game exposing (OtoGame)
import Game.GameStatus exposing (GameStatus(..))
import Game.Round as Round exposing (Round)
import Game.Word exposing (Word)
import Helpers exposing (dropWhile, find, maybeFilter)
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (attribute, class, src, style)
import Route
import User.Avatar as Avatar
import User.Name as Name
import User.Uid exposing (Uid)
import User.User exposing (SimpleInfo)

type Game
    = RightState State
    | WrongState OtoGame

type State
    = Mine LeftPlayer (Maybe RightPlayer) Payload
    | Others LeftPlayer (Maybe RightPlayer) Payload

payload : State -> Payload
payload state =
    case state of
        Mine _ _ p_ -> p_
        Others _ _ p_ -> p_

type Guess
    = NoGuess
    | LeftGuess Word
    | RightGuess Word

type alias Payload =
    { uid : Uid
    , status : GameStatus
    , guess : Guess
    , question : Maybe Round
    , previousRounds : List Round
    }


type alias Partner = SimpleInfo
type alias LeftPlayer = SimpleInfo
type alias RightPlayer = SimpleInfo



fromGame : Uid -> OtoGame -> Game
fromGame me game =
    let
        players = game |> Game.players

        (leftPlayer, rightPlayer) =
            case players |> find (\x -> x.uid == me) of
                Nothing -> (game.player_1, game.player_2)
                Just some -> (some, players |> find (\x -> x.uid /= me))

        rounds = game.words |> Round.fromWords
        firstRound = rounds |> List.head
        question = rounds |> find (not << Round.isIncomplete)

        previousRounds = rounds |> Debug.log "rounds" |> dropWhile(Round.isIncomplete) |> Debug.log "Complete rounds" |> List.tail |> Maybe.withDefault []

        guessFromRound round =
            case round of
                Round.Complete _ _ -> Just NoGuess
                Round.Incomplete w ->
                    if w.player == leftPlayer.uid then
                        Just (LeftGuess w)
                    else
                        case rightPlayer of
                            Nothing -> Nothing
                            Just some ->
                                if w.player == some.uid
                                then Just (RightGuess w)
                                else Nothing

        guess =
            firstRound
            |> maybeFilter (Round.isIncomplete)
            |> Maybe.andThen (guessFromRound)
            |> Maybe.withDefault NoGuess

        payload_ =
            { uid = game.uid
            , status = game.status
            , guess = guess
            , question = question
            , previousRounds = previousRounds
            }

    in
    if game.words |> List.map .player |> List.any (\p -> not <| List.member p (players |> List.map .uid)) then
        WrongState game
    else if rounds |> List.filter(Round.isIncomplete) |> List.length |> (\x -> x > 1) then
        WrongState game
    else if leftPlayer.uid == me then
        RightState (Mine leftPlayer rightPlayer payload_)
    else
        RightState (Others leftPlayer rightPlayer payload_)



isMyTurn : Game -> Bool
isMyTurn game =
    case game of
        RightState state ->
            isMyTurnState state
        _ -> False

isMyTurnState : State -> Bool
isMyTurnState state =
    case state of
        Mine _ _ p_ ->
            case (p_.status, p_.guess) of
                (Open, RightGuess _) -> True
                (Open, NoGuess) -> True
                _ -> False
        _  -> False



isPartnersTurn : Game -> Bool
isPartnersTurn game =
    case game of
            RightState (Mine _ _ p_) ->
                case (p_.status, p_.guess) of
                    (Open, LeftGuess _) -> True
                    _ -> False
            _ -> False

view : Game -> Html msg
view sGame =
    case sGame of
        WrongState _ -> text "TODO: WrongState"
        RightState (Others _ _ _) -> text "TODO: Others Game"
        RightState (Mine me partner p_) ->
            let
                picture =
                    case partner of
                        Just some ->
                            img
                                [ src (Avatar.toString some.avatar)
                                , attribute "referrerpolicy" "no-referrer"
                                , class "rounded-lg"
                                , style "min-width" "48px"
                                ] []
                        Nothing ->
                            span [ class "material-symbols-outlined md-48" ][ text "psychology_alt" ]
                partnerRound =
                    case partner of
                        Just some ->
                            span
                                [ class "ml-2 flex-1 py-3 flex flex-col whitespace-nowrap"]
                                [ span [class "font-bold"] [text (Name.toString some.name)]
                                , p_.question
                                    |> Maybe.map Round.id
                                    |> Maybe.withDefault 0
                                    |> (\x -> "round " ++ (String.fromInt x) )
                                    |> text
                                    |> List.singleton
                                    |> span [class "text-xs uppercase on-surface-variant-text"]
                                ]
                        Nothing ->
                            span
                               [ class "ml-2 flex-1 py-3"]
                               [ span [] [text "Random partner"]
                               ]
                words =
                    case p_.question of
                        Just (Round.Complete a b) ->
                            span [ class "divide-y divide-pink-200 mr-2 flex flex-col text-right font-bold on-surface-variant-text uppercase text-sm overflow-hidden whitespace-nowrap"]
                                [ span [class "py-1"] [ text (a.word)]
                                , span [class "py-1"] [ text (b.word)]
                                ]
                        _ -> text ""
            in
            gameWidget p_.uid (avatar picture) partnerRound words


gameWidget : Uid -> Html msg -> Html msg -> Html msg -> Html msg
gameWidget uid picture partnerRound words =
    a
        [ class "flex flex-row items-center"
        , Route.href (uid |> Route.Game)
        ]
        [ picture
        , partnerRound
        , words
        ]

avatar : Html msg -> Html msg
avatar img =
    span [ class "h-12 w-12 m-2" ] [ img ]