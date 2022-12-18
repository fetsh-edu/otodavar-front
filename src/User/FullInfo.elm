module User.FullInfo exposing (..)

import Game.OtoGame as OtoGame exposing (OtoGame)
import Json.Decode as Decode exposing (Decoder)
import User.Avatar as Avatar exposing (Avatar)
import User.Email as Email exposing (Email)
import User.FriendStatus as FriendStatus
import User.Handle as Handle exposing (Handle)
import User.Name as Name exposing (Name)
import User.Uid as Uid exposing (Uid)
import User.User exposing (SimpleInfo, decoderInfo)

type alias FullInfo =
    { email : Email
    , uid : Uid
    , handle : Handle
    , avatar : Avatar
    , name : Name
    , friendStatus : FriendStatus.Status
    , gamesCount : Int
    , friendsCount : Int
    , friends : Maybe (List SimpleInfo)
    , incomingFriendRequests : Maybe (List SimpleInfo)
    , outgoingFriendRequests : Maybe (List SimpleInfo)
    , commonOpenGames : Maybe (List OtoGame)
    , commonClosedGames : Maybe (List OtoGame)
    }


decoderFullInfo : Decoder FullInfo
decoderFullInfo =
    Decode.succeed FullInfo
         |> Decode.map2 (|>) (Decode.field "email" Email.decoder)
         |> Decode.map2 (|>) (Decode.field "uid" Uid.decoder)
         |> Decode.map2 (|>) (Decode.field "user_name" Handle.decoder)
         |> Decode.map2 (|>) (Decode.field "avatar" Avatar.decoder)
         |> Decode.map2 (|>) (Decode.field "name" Name.decoder)
         |> Decode.map2 (|>) (Decode.field "friend_status" FriendStatus.decoder)
         |> Decode.map2 (|>) (Decode.field "games_count" Decode.int)
         |> Decode.map2 (|>) (Decode.field "friends_count" Decode.int)
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "friends" (Decode.list decoderInfo)))
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "incoming_friends" (Decode.list decoderInfo)))
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "outgoing_friends" (Decode.list decoderInfo)))
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "common_open_games" (Decode.list OtoGame.decoder)))
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "common_closed_games" (Decode.list OtoGame.decoder)))