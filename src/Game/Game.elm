module Game.Game exposing (..)

import Game.OtoGame as Game exposing (OtoGame)
import Game.GameStatus as Status exposing (GameStatus(..))
import Game.Round as Round exposing (Round)
import Game.Stamp as Stamp
import Game.Word as Word exposing (Word)
import Helpers exposing (dropWhile, find, maybeFilter)
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (attribute, class, src, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (WebData)
import Route
import User.Avatar as Avatar
import User.Name as Name
import User.Uid exposing (Uid)
import User.User exposing (SimpleInfo)
import View.Helper

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

leftPlayer : State -> LeftPlayer
leftPlayer state =
    case state of
        Mine l_ _ _ -> l_
        Others l_ _ _ -> l_


opponent : State -> Maybe RightPlayer
opponent state =
    case state of
        Mine _ p_ _ -> p_
        Others _ p_ _ -> p_

uid : Game -> Uid
uid game =
    case game of
        RightState state -> (payload state).uid
        WrongState otoGame -> otoGame.uid

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


updateWord : Word -> Game -> Game
updateWord word game =
    case game of
        RightState state ->
            case state of
                Mine l_ mr_ payload_ ->
                    RightState (Mine l_ mr_ (updateWordInPayload word payload_))
                Others l_ mr_ payload_ ->
                    RightState (Others l_ mr_ (updateWordInPayload word payload_))
        WrongState otoGame ->
            WrongState (Game.updateWord word otoGame)


updateWordInPayload : Word -> Payload -> Payload
updateWordInPayload word payload_ =
    { payload_
    | question = Maybe.map (Round.updateWord word) payload_.question
    , previousRounds = List.map (Round.updateWord word) payload_.previousRounds
    }



type alias Partner = SimpleInfo
type alias LeftPlayer = SimpleInfo
type alias RightPlayer = SimpleInfo

fromGame : Maybe Uid -> OtoGame -> Game
fromGame maybeMe game =
    let
        players = game |> Game.players
        (leftPlayer_, rightPlayer) =
            case maybeMe |> Maybe.andThen (\me -> players |> find (\x -> x.uid == me)) of
                Nothing -> (game.player_1, game.player_2)
                Just some -> (some, players |> find (\x -> x.uid /= some.uid))

        rounds = game.words |> Round.fromWords leftPlayer_.uid
        firstRound = rounds |> List.head
        question = rounds |> find (not << Round.isIncomplete)

        previousRounds = rounds |> dropWhile(Round.isIncomplete) |> List.tail |> Maybe.withDefault []

        guessFromRound round =
            case round of
                Round.Complete _ _ -> Just NoGuess
                Round.Incomplete w ->
                    if w.player == leftPlayer_.uid then
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
    else if maybeMe |> Maybe.map (\me -> leftPlayer_.uid == me) |> Maybe.withDefault False then
        RightState (Mine leftPlayer_ rightPlayer payload_)
    else
        RightState (Others leftPlayer_ rightPlayer payload_)



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

gamePreview : Game -> Html msg
gamePreview sGame =
    case sGame of
        WrongState _ -> text "Game is in some wrong state. It shouldn't be possible. If you can, send this url to developer."
        RightState (Others _ _ _) -> text "TODO: Others Game"
        RightState (Mine _ partner p_) ->
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
                                [ class "ml-2 flex-1 py-3 flex flex-col whitespace-nowrap overflow-hidden fade-right relative"]
                                [ span [class "font-bold"] [text (Name.toString some.name)]
                                , p_
                                    |> .question
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
                            case p_.status of
                                Open ->
                                    span [ class "divide-y divide-light mr-2 flex flex-col text-right font-bold on-surface-variant-text uppercase text-sm overflow-hidden whitespace-nowrap"]
                                        [ span [class "py-1"] [ text (a.word)]
                                        , span [class "py-1"] [ text (b.word)]
                                        ]
                                Closed ->
                                    span [ class "mr-2 flex items-center text-right on-surface-variant-text overflow-hidden whitespace-nowrap"]
                                        [ span [ class "text-base mr-1 material-symbols-outlined opacity-40" ] [ text "done_all"]
                                        , span [class "py-1 font-bold uppercase text-sm  font-bold"] [ text (a.word)]
                                        ]
                        _ -> text ""
            in
            gameWidget p_.uid (avatar picture) partnerRound words

type alias Translator msg =
    { playAgainMsg : (Uid -> msg)
    , closeStickersMsg : msg
    , stampSelectMsg : (Stamp.Stamp -> msg)
    , createStampMsg : (Int -> msg)
    , submitGuessMsg : msg
    , onGuessChangeMsg : (String -> msg)
    }

gameView : Translator msg -> Maybe { b | wordId : c } -> String -> WebData Game -> State -> Bool -> List (Html msg)
gameView translator stickerSelect guessText guessData state sticky =
    let
        stickersSelect =
            case stickerSelect of
                Nothing -> text ""
                Just { wordId } ->
                    div [ class "absolute top-2 w-full select-none" ]
                        [ div [class "surface on-surface-text rounded-lg p-4 flex flex-wrap mx-2 opacity-95 justify-center"]
                            (List.map (\x -> Stamp.selectItem (translator.stampSelectMsg x) x) Stamp.all)
                        , span
                            [ onClick translator.closeStickersMsg
                            , class "absolute top-0 right-2 p-2 material-symbols-outlined md-24 cursor-pointer"
                            ]
                            [ text "close" ]
                        ]
    in
    [ div
        [ class "secondary-container on-secondary-container-text rounded-lg relative mb-2" ]
        [ currentGuess translator guessText state guessData
        , div [ class "px-4 pb-4"] <| List.map (Round.view translator.createStampMsg Round.Small) <| .previousRounds <| payload <| state
        , stickersSelect
        ]
    , case (state |> payload |> .status, state |> opponent) of
        (Closed, Just opponent_) ->
            View.Helper.playAgainButton (translator.playAgainMsg opponent_.uid) sticky
        _ -> text ""
    ]

currentGuess : Translator msg -> String -> State -> WebData Game -> Html msg
currentGuess translator guessText sGame guessData =
    let
        speechBubble =
            case sGame of
                Others _ _ _ -> text ""
                Mine _ _ p ->
                    div
                        [ class "px-3"
                        , case guessData of
                          RemoteData.Loading -> class "animate-pulse"
                          RemoteData.Failure _ -> class ""
                          _ -> class ""
                        ]
                        [ div
                            [ class "flex z-10 mt-2 surface-7 on-surface-text text-sm p-2 pl-3 w-full h-12 rounded-lg filter drop-shadow speech"
                            , if (payload sGame |> .status) == Status.Closed
                              then class "speech-left speech-right"
                              else class "speech-left"
                            ] (bubbleContent p)
                        ]

        bubbleContent : Payload -> List (Html msg)
        bubbleContent p =
            case p.status of
                Status.Closed ->
                    case p.question of
                        Nothing -> [text ""]
                        Just (Round.Incomplete _) -> [text ""]
                        Just (Round.Complete w _) ->
                            [ div [ class "flex w-full justify-center items-center font-medium text-lg truncate overflow-ellipses"] [span [class "uppercase"] [text w.word]]]
                Status.Open ->
                    case (p.guess, guessData) of
                        (LeftGuess w, _) ->
                            [ div [ class "flex w-full justify-center items-center font-medium text-lg truncate overflow-ellipses"] [span [class "uppercase"] [text w.word]]]
                        (_, RemoteData.Loading) ->
                            [ div [ class "flex w-full justify-center items-center font-medium text-lg truncate overflow-ellipses"] [span [class "uppercase"] [text guessText]]]
                        _ ->
                            let
                                button_ =
                                    if  guessText |> String.trim |> String.isEmpty
                                    then text ""
                                    else
                                        Html.button
                                            [ class "flex-none primary-container on-primary-container-text rounded-lg px-4 py-1 font-bold mr-0 ml-1"
                                            , onClick translator.submitGuessMsg
                                            , Html.Attributes.disabled (guessText |> String.trim |> String.isEmpty)
                                            ]
                                            [ text "Say It!" ]
                            in
                            [ Html.input
                                [ Html.Events.onInput translator.onGuessChangeMsg
                                , Helpers.onEnter translator.submitGuessMsg
                                , Html.Attributes.autofocus True
                                , Html.Attributes.id "word-input"
                                , Html.Attributes.autocomplete False
                                , class "flex-grow border-0 shy bg-transparent"
                                , style "appearance" "none"
                                , Html.Attributes.placeholder "SAY IT HERE..."
                                , Html.Attributes.value guessText  ] []
                            , button_
                            ]

    in
    div
        [ class "tertiary-container on-tertiary-container-text rounded-lg py-4 filter drop-shadow overflow-hidden"]
        [
        -- AVATARS
        div [class "flex z-10 flex-row justify-around"]
            [ span
                [ class "flex items-center w-40 flex-col truncate overflow-ellipses" ]
                [ Avatar.img (leftPlayer sGame).avatar "w-20 h-20  filter drop-shadow"
                , span
                    []
                    [ case sGame of
                      Mine _ _ _ -> text ""
                      Others l _ _ -> text <| Name.toString l.name
                    ]
                ]
            , span
                [ class "flex items-center w-40 flex-col z-1 relative" ]
                [ case opponent sGame of
                    Just user -> a [ Route.href (user.uid |> Route.Profile)] [Avatar.img user.avatar "w-20 h-20  filter drop-shadow"]
                    Nothing -> span [ class "surface w-20 h-20 filter drop-shadow rounded-lg flex items-center justify-center"] [span [ class "material-symbols-outlined md-72" ][ text "psychology_alt" ]]
                , if (payload sGame).status == Status.Closed
                    then text View.Helper.nbsp
                    else span
                            [ class "pt-2 text-sm on-surface-variant-text truncate overflow-ellipses" ]
                            [ opponent sGame |> Maybe.map (.name >> Name.toString) |> Maybe.withDefault "Random user" |> text
                            ]
                -- READY CLOUD
                , case (payload sGame).guess of
                  RightGuess _ ->
                      span [ class "thought absolute px-3 py-1 surface text-sm surface-7 on-surface-text -left-4 top-0"] [ text "ready"]
                  _ -> text ""
                ]
            ]
        -- WIN ICON
        , if (payload sGame |> .status) == Status.Closed
          then
              div
                  [ class "flex justify-center items-center absolute w-full top-8", style "z-index" "-1"]
                  [ span
                      [ class "surface-2 on-surface-text rounded-full py-3 px-4 text-2xl tracking-tight font-bold"]
                      [ span [ class "material-symbols-outlined font-bold text-3xl" ] [ text "done_all"]]
                      ]
          else text ""
        -- GLOW
        , if (payload sGame |> .status) == Status.Closed
            then div [class "absolute win-glow h-96 w-full -top-10 left-0", style "z-index" "-1"][]
            else text ""
        , speechBubble
        -- TODO : Add errors
        , div [] []
        -- QUESTION
        , if (payload sGame).status == Status.Closed
          then text ""
          else
              case payload sGame |> .question of
                  Just a ->
                      Round.view translator.createStampMsg Round.Big a
                  Nothing ->
                      case sGame of
                          Mine _ _ p_ ->
                              case p_.guess of
                                  LeftGuess _ -> Html.p [ class "text-center mt-4"] [ text "Now it's partner's turn" ]
                                  _ -> Html.p [ class "text-center mt-4"] [ text "Say your first word, any word!" ]
                          Others _ _ _ -> text ""
        ]


gameWidget : Uid -> Html msg -> Html msg -> Html msg -> Html msg
gameWidget uid_ picture partnerRound words =
    a
        [ class "flex flex-row items-center invisible-click"
        , Route.href (uid_ |> Route.Game)
        ]
        [ picture
        , partnerRound
        , words
        ]

avatar : Html msg -> Html msg
avatar img =
    span [ class "h-12 w-12 m-2" ] [ img ]


decoder : Maybe Uid -> Decoder Game
decoder uid_ =
    Decode.map (fromGame uid_) Game.decoder