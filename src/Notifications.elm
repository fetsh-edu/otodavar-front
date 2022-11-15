module Notifications exposing (..)

import Html exposing (Html, a, p, text)
import Html.Attributes exposing (class)
import Iso8601
import Json.Decode as Decode exposing (Decoder, list)
import Json.Encode as Encode exposing (Value)
import OtoApi exposing (config)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Route
import User.Bearer as Bearer exposing (Bearer)
import Time
import User.Name exposing (Name(..))
import User.Uid as Uid exposing (Uid(..))

type alias Notifications =
    { shown : Bool
    , items : WebData (List Notification)
    }

type alias Notification =
    { seen : Bool
    , created : Time.Posix
    , id : Int
    , payload : Payload
    }

type Payload
    = FriendRequest Uid Name
    | FriendAccept Uid Name
    | Unknown String


type Msg
    = GotNotifications (WebData (List Notification))
    | NoOp

initModel =
    { shown = False
    , items = NotAsked
    }


decoder : Decoder Notification
decoder =
    Decode.map4 Notification
        (Decode.field "seen" (Decode.bool))
        (Decode.field "created_at" (Iso8601.decoder))
        (Decode.field "id" Decode.int)
        (Decode.field "payload" payloadDecoder)

--friendRequestDecoder : Decoder Payload
--friendRequestDecoder =
--
--            |> Decode.andThen (\x -> x)


payloadDecoder : Decoder Payload
payloadDecoder =
    Decode.oneOf
        [ Decode.field "action" Decode.string
            |> Decode.andThen
                (\action ->
                    case action of
                        "friend_request" -> Decode.map2 FriendRequest |> friendRequestDecoder
                        "friend_accept" -> Decode.map2 FriendAccept |> friendRequestDecoder
                        str -> Decode.succeed (Unknown ("Action: " ++ str))
                )
        , keyValuePairs |> Decode.map Unknown
        , Decode.succeed (Unknown "Weird format")
        ]


keyValuePairs : Decoder String
keyValuePairs =
    Decode.string
        |> Decode.keyValuePairs
        |> Decode.map (List.map (\(a,b) -> a ++ " : " ++ b) >> String.join ", ")


friendRequestDecoder a =
    a
        (Decode.map Uid (Decode.field "uid" Decode.string))
        (Decode.map Name (Decode.field "name" Decode.string))



get : (WebData (List Notification) -> msg) -> Maybe Bearer -> Cmd msg
get toMsg bearer =
    let
        url = OtoApi.routes.notifications.index
        message bearer_ = RemoteData.Http.getWithConfig (config bearer_) url toMsg (Decode.list decoder)
    in
    bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


markAsSeen : (WebData (List Notification) -> msg) -> Maybe Bearer -> Int -> Cmd msg
markAsSeen toMsg bearer id =
    let
        url = OtoApi.routes.notifications.markAsSeen
        message bearer_ = RemoteData.Http.postWithConfig (config bearer_) url toMsg (Decode.list decoder) (encode { id = id })
    in
    bearer |> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


encode : { a | id : Int } -> Value
encode { id } =
    Encode.object
        [ ( "id", Encode.int id )
        ]



view : Notification -> Html msg
view notification =
    let
        bold =
            if notification.seen then
                ""
            else
                "font-semibold"
        text_ =
            case notification.payload of
                FriendRequest uid name -> "FriendRequest: " ++ (User.Name.toString name)
                FriendAccept uid name -> "FriendAccept: " ++ (User.Name.toString name)
                Unknown string -> string
        href_ =
            case notification.payload of
                FriendRequest uid name ->
                    Route.href (uid |> Route.Profile)

                FriendAccept uid name ->
                    Route.href (uid |> Route.Profile)

                Unknown string ->
                    class ""


    in
    p [ class "text-sm text-gray-500 p-2", class bold] [ a [ href_ ] [text text_] ]