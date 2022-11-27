module View.Helper exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)

smallContainer : String -> Html msg
smallContainer text_ =
    div
        [ class "flex flex-col m-10 justify-center items-center animate-pulse"]
        [ div
            [ class "transition-transform transform w-full md:w-1/2 surface-1 on-surface-text rounded-lg flex flex-row p-4 mb-8 text-lg shadow-md justify-center items-center" ]
            [ span
                [ class "animate-spin flex justify-center items-center h-14 w-14 material-symbols-outlined mr-0" ]
                [ text "refresh"]
            , span [ class "pl-0 overflow-ellipsis overflow-hidden"] [text text_]
            ]
        ]


container : List (Html msg) -> Html msg
container = div [class "profile-page container mx-auto px-4 mt-6 md:mt-28 md:max-w-5xl"]

section : String -> String -> List (Html msg) -> Html msg
section title_ class_ list =
     div
         [ class "relative flex flex-col min-w-0 break-words w-full mb-6 shadow-xl rounded-lg surface-1 on-surface-text"]
         [ div
            [ class "rounded-t-lg py-2 px-4 font-bold"
            , class class_
            ]
            [ text title_ ]
         , div [ class "divide-y divide-pink-200"] list
         ]