module Game.Stamp exposing (..)

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