module Game.Stamp exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
type Stamp
    = Nothing
    | Lol
    | Omg
    | Wtf
    | Almost
    | Aaaaa
    | Monkey
    | Love
    | Party
    | ButHow

decoder : Decode.Decoder Stamp
decoder =
    Decode.string |> Decode.map fromString

encode : Stamp -> Encode.Value
encode stamp =
    Encode.string
        (case  stamp of
            Nothing -> "nothing"
            Lol -> "lol"
            Omg -> "omg"
            Wtf -> "wtf"
            Almost -> "almost"
            Aaaaa -> "aaaaa"
            Monkey -> "monkey"
            Love -> "love"
            Party -> "party"
            ButHow -> "buthow"
        )



all : List Stamp
all =
    [ Lol
    , Omg
    , Wtf
    , Almost
    , Aaaaa
    , Monkey
    , Love
    , Party
    , ButHow
    ]

fromString : String -> Stamp
fromString str =
    case str of
        "nothing" -> Nothing
        "lol" -> Lol
        "omg" -> Omg
        "wtf" -> Wtf
        "almost" -> Almost
        "aaaaa" -> Aaaaa
        "monkey" -> Monkey
        "love" -> Love
        "party" -> Party
        "buthow" -> ButHow
        _ -> Nothing

toString : Stamp -> String
toString str =
    case str of
        Nothing -> ""
        Lol -> "lol"
        Omg -> "omg"
        Wtf -> "wtf"
        Almost -> "almost"
        Aaaaa -> "AAAAAA!"
        Monkey -> "noooo!"
        Love -> "love"
        Party -> "whoo-hoo!"
        ButHow -> "..but how?"

toIcon : Stamp -> String
toIcon str =
    case str of
        Nothing -> ""
        Lol -> "😂"
        Omg -> "🤯"
        Wtf -> "🧐"
        Almost -> "🙏"
        Aaaaa -> "🤌"
        Monkey -> "🙈"
        Love -> "😍"
        Party -> "🥳"
        ButHow -> "🤷"

--  VIEW

selectItem : msg -> Stamp -> Html msg
selectItem some stamp =
    Html.span
        [ Html.Attributes.class "border border-light-200 border-dashed p-1 px-2 m-1 rounded-md cursor-pointer select-none"
        , Html.Events.onClick some
        ]
        (List.map Html.text [ toIcon stamp, " ", toString stamp ])

smallView : List (Html.Attribute msg) -> Stamp -> Html msg
smallView classes stamp =
    case stamp of
        Nothing -> Html.text ""
        _ ->
            Html.span
                ([ Html.Attributes.class "text-sm px-0 py-0 rounded-lg opacity-50"
                ] ++ classes)
                [ Html.text (toIcon stamp)
                ]

bigView : List (Html.Attribute msg) -> Stamp -> Html msg
bigView classes stamp =
    case stamp of
        Nothing -> Html.text ""
        _ ->
            Html.span
                ([ Html.Attributes.class "text-sm surface on-surface-text px-2 py-0 pb-1 rounded-lg opacity-60 select-none"
                ] ++ classes)
                (List.map Html.text [ toIcon stamp, " ", toString stamp ])