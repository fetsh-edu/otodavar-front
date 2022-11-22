module Game.SomeonesGame exposing (..)

import Game.Game as Game exposing (Game)
import Game.GameStatus exposing (GameStatus(..))
import Game.Word exposing (Word)
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (attribute, class, src, style)
import Route
import User.Avatar as Avatar
import User.Name as Name
import User.Uid exposing (Uid)
import User.User exposing (SimpleInfo)

type SomeonesGame
    = Mine Me (Maybe Partner) Game
    | Others Game

type Partner = Partner SimpleInfo
type alias Me = Uid

fromGame : Me -> Game -> SomeonesGame
fromGame me game =
    let
        players = game |> Game.players
    in
    if players |> List.map .uid |> List.member me
    then Mine me (players |> List.filter (\x -> x.uid /= me) |> List.head |> Maybe.map Partner) game
    else Others game

toGame : SomeonesGame -> Game
toGame sGame =
    case sGame of
        Mine _ _ game -> game
        Others game -> game

lastWorOfMyOpenGame : (Me -> Word -> Bool) -> Bool -> SomeonesGame -> Bool
lastWorOfMyOpenGame fun def sGame =
    case sGame of
            Others _ -> False
            Mine me partner game ->
                case game.status of
                    Closed -> False
                    Open ->
                        case Game.rounds game.words of
                            (lastWord, []) :: _ -> fun me lastWord
                            _                   -> def


questionRound : SomeonesGame -> Maybe Game.Round
questionRound game =
    game
        |> toGame
        |> .words
        |> Game.rounds
        |> List.map Game.toRound
        |> List.filter ( \x ->
            case x of
                Game.Complete _ _ -> True
                Game.Incomplete _ -> False
        )
        |> List.head



isMyTurn : SomeonesGame -> Bool
isMyTurn =
    lastWorOfMyOpenGame (\me lastWord -> lastWord.player /= me) True


isPartnersTurn : SomeonesGame -> Bool
isPartnersTurn =
    lastWorOfMyOpenGame (\me lastWord -> lastWord.player == me) False

view : SomeonesGame -> Html msg
view sGame =
    case sGame of
        Others _ -> text "Not implemented"
        Mine me partner game ->
            let
                picture =
                    case partner of
                        Just (Partner some) ->
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
                        Just (Partner some) ->
                            span
                                [ class "ml-2 flex-1 py-3 flex flex-col whitespace-nowrap"]
                                [ span [class "font-bold"] [text (Name.toString some.name)]
                                , game.words
                                    |> Game.rounds
                                    |> List.head
                                    |> Maybe.map (\(w, _) -> w.roundId |> String.fromInt)
                                    |> Maybe.withDefault "0"
                                    |> (\x -> "round " ++ x )
                                    |> text
                                    |> List.singleton
                                    |> span [class "text-xs uppercase text-gray-500"]
                                ]
                        Nothing ->
                            span
                               [ class "ml-2 flex-1 py-3"]
                               [ span [] [text "Random partner"]
                               ]
                qr = questionRound sGame
                words =
                    case qr of
                        Just (Game.Complete a b) ->
                            span [ class "divide-y divide-pink-200 mr-2 flex flex-col text-right font-bold on-surface-variant-text uppercase text-sm overflow-hidden whitespace-nowrap"]
                                [ span [class "py-1"] [ text (a.word)]
                                , span [class "py-1"] [ text (b.word)]
                                ]
                        _ -> text ""
            in
            gameWidget game.uid (avatar picture) partnerRound words



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