port module Notifications exposing (..)

import Html exposing (Html, a, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Iso8601
import Json.Decode as Decode exposing (Decoder, Error)
import Json.Encode as Encode exposing (Value)
import OtoApi exposing (config)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Route
import Url exposing (Url)
import User.Bearer as Bearer exposing (Bearer)
import Time
import User.Name exposing (Name(..))
import User.Uid exposing (Uid(..))

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
    | GameCreated Uid Name
    | GameAccepted Uid Name
    | Unknown String


type Msg
    = GotNotifications (WebData (List Notification))
    | GotNotification (Result Error Notification)

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
                        "game_created" -> Decode.map2 GameCreated |> friendRequestDecoder
                        "game_accepted" -> Decode.map2 GameAccepted |> friendRequestDecoder
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



get : Url -> (WebData (List Notification) -> msg) -> Maybe Bearer -> Cmd msg
get apiUrl toMsg bearer =
    let
        url = (OtoApi.routes apiUrl).notifications.index
        message bearer_ = RemoteData.Http.getWithConfig (config bearer_) url toMsg (Decode.list decoder)
    in
    bearer|> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


markAsSeen : Url -> (WebData (List Notification) -> msg) -> Maybe Bearer -> Int -> Cmd msg
markAsSeen apiUrl toMsg bearer id =
    let
        url = (OtoApi.routes apiUrl).notifications.markAsSeen
        message bearer_ = RemoteData.Http.postWithConfig (config bearer_) url toMsg (Decode.list decoder) (encode { id = id })
    in
    bearer |> Maybe.map (message << Bearer.toString) |> Maybe.withDefault Cmd.none


encode : { a | id : Int } -> Value
encode { id } =
    Encode.object
        [ ( "id", Encode.int id )
        ]


view : { onExit : msg } -> Notification -> Html msg
view { onExit } notification =
    let
        bold =
            if notification.seen then
                ""
            else
                "font-semibold"
        text_ =
            case notification.payload of
                FriendRequest uid name ->
                    [ span [ class "font-bold" ] [ text (User.Name.toString name)], text " wants to be your friend"]
                FriendAccept uid name ->
                    [ span [ class "font-bold" ] [ text (User.Name.toString name)], text " accepted your friend request"]
                Unknown string -> [text string]
                GameCreated uid name ->
                    [ span [ class "font-bold" ] [ text (User.Name.toString name)], text " started a game with you"]
                GameAccepted uid name ->
                    [ span [ class "font-bold" ] [ text (User.Name.toString name)], text " accepted your random game"]
        icon_ =
            case notification.payload of
                FriendRequest uid name -> "person_add"
                FriendAccept uid name -> "person"
                GameCreated uid name -> "sports_esports"
                GameAccepted uid name -> "sports_esports"
                Unknown string -> ""

        href_ =
            case notification.payload of
                FriendRequest uid name ->
                    Route.href (uid |> Route.Profile)

                FriendAccept uid name ->
                    Route.href (uid |> Route.Profile)

                GameCreated uid name ->
                    Route.href (uid |> Route.Game)

                GameAccepted uid name ->
                    Route.href (uid |> Route.Game)

                Unknown string ->
                    class ""
    in
    a
        [ href_, onClick onExit, class "flex flex-row items-center text-sm on-surface-variant-text p-2", class bold ]
        [ span [ class "material-symbols-outlined md-24 mr-2"] [text icon_ ], span [ class "align-baseline "] text_ ]


port onNotification : (Encode.Value -> msg) -> Sub msg