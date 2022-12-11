module Notifications.TelegramAuth exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Optional as Opt

type alias TelegramNotifications = Maybe TelegramAuth

type alias TelegramAuth =
     { id : Int
     , first_name : Maybe String
     , last_name : Maybe String
     , username : Maybe String
     , photo_url : Maybe String
     , auth_date : Int
     , hash : String
     }

telegramDataDecoder : Decode.Decoder TelegramAuth
telegramDataDecoder =
    Decode.map7 TelegramAuth
        (Decode.at ["detail", "id"] Decode.int)
        (Decode.maybe (Decode.at ["detail", "first_name"] Decode.string))
        (Decode.maybe (Decode.at ["detail", "last_name"] Decode.string))
        (Decode.maybe (Decode.at ["detail", "username"] Decode.string))
        (Decode.maybe (Decode.at ["detail", "photo_url"] Decode.string))
        (Decode.at ["detail", "auth_date"] Decode.int)
        (Decode.at ["detail", "hash"] Decode.string)

encode : TelegramAuth -> Encode.Value
encode auth =
    [ ( "id", auth.id) |> Opt.field Encode.int
    , ( "first_name", auth.first_name) |> Opt.optionalField Encode.string
    , ( "last_name", auth.last_name) |> Opt.optionalField Encode.string
    , ( "username", auth.username) |> Opt.optionalField Encode.string
    , ( "photo_url", auth.photo_url) |> Opt.optionalField Encode.string
    , ( "auth_date", auth.auth_date) |> Opt.field Encode.int
    , ( "hash", auth.hash) |> Opt.field Encode.string
    ] |> Opt.objectMaySkip