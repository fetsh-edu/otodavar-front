module View.Helper exposing (..)

import Html exposing (Html, a, button, div, p, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)


loadingContainer : String -> Html msg
loadingContainer text_ =
    smallContainer (class "animate-pulse")
        [ span
            [ class "animate-spin flex justify-center items-center h-14 w-14 material-symbols-outlined mr-0" ]
            [ text "refresh"]
        , span [ class "pl-0 overflow-ellipsis overflow-hidden"] [text text_]
        ]


simpleSmallContainer : List (Html msg) -> Html msg
simpleSmallContainer = smallContainer (class "")


smallContainer : Html.Attribute msg -> List (Html msg) -> Html msg
smallContainer class_ content_ =
    div
        [ class "flex flex-col m-10 justify-center items-center"
        , class_]
        [ div
            [ class "transition-transform transform w-full md:w-1/2 surface-1 on-surface-text rounded-lg flex flex-row p-4 mb-8 text-lg shadow-md justify-center items-center"
            ]
            content_
        ]

notFound : Html msg
notFound = errorView [ text "404"] [ text "Sliha, not found" ]


errorView : List (Html msg) -> List (Html msg) -> Html msg
errorView header_ description_ =
    simpleSmallContainer
        [ div [ class "flex flex-col items-center py-4"]
            [ div [ class "font-black text-9xl mb-4 on-surface-variant-text"] header_
            , div [ class " on-surface-variant-text" ] description_
            ]
        ]


container : List (Html msg) -> Html msg
container = container_ []

container_ : List (Html.Attribute msg) -> List (Html msg) -> Html msg
container_ classes =
    div
        ([ class "profile-page container mx-auto px-4 mt-6 sm:mt-14 md:mt-18 md:max-w-5xl pb-4"
        ] ++ classes)

section : String -> String -> List (Html msg) -> Html msg
section title_ class_ list =
     div
         [ class "relative flex flex-col min-w-0 break-words w-full mb-6 shadow-xl rounded-lg surface-1 on-surface-text"]
         [ div
            [ class "rounded-t-lg py-2 px-4 font-bold"
            , class class_
            ]
            [ text title_ ]
         , div [ class "divide-y divide-light"] list
         ]


about : Html msg -> List (Html msg)
about game = [okGo, description, game, rules]

okGo : Html msg
okGo =
    section
        "The game" "secondary-container secondary-text uppercase text-center"
        [ div
            [ class "p-4 sm:p-8 text-sm text" ]
            [ p [ class "pb-4" ]
                [ text "Almost ten years ago the music band “OK Go” released a game called "
                , a [ class "underline primary-text", target "_blank", href "https://www.youtube.com/watch?v=2sP1DqyagXE" ] [ text "“Say the Same Thing”" ]
                , text ". It was greatly entertaining and addictive but then vanished. So here we have an hommage."
                ]
            ]
        ]

description: Html msg
description =
    section
        "אותו דבר" "primary-container primary-text uppercase text-center"
        [ div
            [ class "p-4 sm:p-8 text-sm text" ]
            [ p [ class "pb-4" ] [ text "Oto|davar (Hebrew: אותו דבר, lit. \"same thing\") — is a cooperative game of words where every player wins, and no one loses. All you have to do is say the same word with your partner." ]
            ]
        ]

rules : Html msg
rules =
    section
        "Rules" "primary-container primary-text uppercase text-center"
        [ div
            [ class "p-4 sm:p-8 text-sm text" ]
            [ p [ class "pb-4" ] [ text "The rules are simple: you start by saying a word. Any word. A random word. So as your partner. You’ll have an accidental pair of words. And now you have your round zero, where all the fun begins." ]
            , p [ class "pb-4" ] [ text "Just go again. But this time try to say the same word with your partner: find something that connects the first two words, something they have in common. Or even something you think your partner would think they have in common 🙂"]
            , p [ class "pb-4" ] [ text "It’s fun to see the way someone else thinks. You’ll smile, you’ll have facepalms, you’ll laugh, and even scream in frustration!"]
            ]
        ]

-- BUTTONS
playAgainButton : msg -> Html msg
playAgainButton msg =
    div
        [ class "sticky bottom-0 right-0 left-0 container w-full" ]
        [ span
            [ class "justify-center flex w-full" ]
            [ span
                [ class "border-surface border-4 surface rounded mb-2"]
                    [ button
                        [ class "cursor-pointer font-bold inline-block flex items-center leading-normal uppercase text-md rounded outline-none focus:outline-none filter drop-shadow primary on-primary-text px-4 py-2 m-0"
                        , onClick msg
                        ]
                        [ span [ class "material-symbols-outlined text-md mr-2" ][ text "sports_esports" ]
                        , text "Play again"
                        ]
                    ]
            ]
        ]


nbsp : String
nbsp = String.fromChar '\u{00A0}'