module Notifications.TelegramAuth exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode

type alias TelegramNotifications = Maybe TelegramAuth

type alias TelegramAuth =
     { id : Int }

telegramDataDecoder : Decode.Decoder TelegramAuth
telegramDataDecoder =
    Decode.map TelegramAuth
        (Decode.at ["detail", "id"] Decode.int)

encode : TelegramAuth -> Encode.Value
encode = .id >> Encode.int