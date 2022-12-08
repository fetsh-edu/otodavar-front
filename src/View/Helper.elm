module View.Helper exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)


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

nbsp : String
nbsp = String.fromChar '\u{00A0}'