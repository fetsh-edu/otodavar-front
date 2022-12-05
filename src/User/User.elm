module User.User exposing (..)

import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (attribute, class, src)
import Json.Decode as Decode exposing (Decoder, at)
import Json.Encode as Encode exposing (Value)
import User.Avatar as Avatar exposing (Avatar)
import User.Bearer as Bearer exposing (Bearer)
import User.Email as Email exposing (Email)
import User.FriendStatus as FriendStatus
import User.Name as Name exposing (Name)
import User.Uid as Uid exposing (Uid)

type User
    = User Internals

type alias Internals =
    { bearer : Bearer
    , info : SimpleInfo
    }

type alias SimpleInfo =
    { email : Email
    , uid : Uid
    , avatar : Avatar
    , name : Name
    , friendStatus : FriendStatus.Status
    , telegramId : Maybe Int
    }

type alias FullInfo =
    { email : Email
    , uid : Uid
    , avatar : Avatar
    , name : Name
    , friendStatus : FriendStatus.Status
    , gamesCount : Int
    , friendsCount : Int
    , friends : Maybe (List SimpleInfo)
    , incomingFriendRequests : Maybe (List SimpleInfo)
    , outgoingFriendRequests : Maybe (List SimpleInfo)
    }


build : Bearer -> SimpleInfo -> User
build a b = User (Internals a b)

bearer : User -> Bearer
bearer (User intern) =
    intern.bearer

info : User -> SimpleInfo
info (User intern) =
    intern.info

updateInfo : SimpleInfo -> User -> User
updateInfo newInfo (User oldUser) =
    User { oldUser | info = newInfo }

view : User -> Html msg
view user =
        div [ class "transition-transform transform  w-full md:w-1/2 surface-1 on-surface-text rounded-lg flex flex-row p-4 mb-8 text-lg shadow-md justify-left items-center" ]
            [ img [ attribute "referrerpolicy" "no-referrer", class "w-14 h-14 rounded-lg", src (info user |> .avatar |> Avatar.toString)] []
            , span [ class "pl-4 overflow-ellipsis overflow-hidden"] [text (info user |> .name |> Name.toString)]
            ]


-- SERIALIZATION


encode : User -> Value
encode (User intern) =
    Encode.object
        [ ( "bearer", Bearer.encode intern.bearer )
        , ( "info", encodeUserInfo intern.info )
        ]

decoder : Decoder User
decoder =
    Decode.map User
        (Decode.map2 Internals
            (Decode.field "bearer" Bearer.decoder)
            (Decode.field "info" decoderUserInfo))

decoderNullable : Decoder (Maybe User)
decoderNullable = Decode.nullable decoder

encodeUserInfo : SimpleInfo -> Value
encodeUserInfo uInfo =
    Encode.object
        [ ( "email", Email.encode uInfo.email)
        , ( "uid", Uid.encode uInfo.uid)
        , ( "avatar", Avatar.encode uInfo.avatar )
        , ( "name", Name.encode uInfo.name )
        , ( "friend_status", FriendStatus.encode uInfo.friendStatus )
        , ( "telegram_id",
            case uInfo.telegramId of
                Nothing -> Encode.null
                Just some -> Encode.int some
          )
        ]


decoderUserInfo : Decoder SimpleInfo
decoderUserInfo =
    Decode.map6 SimpleInfo
        (Decode.field "email" Email.decoder)
        (Decode.field "uid" Uid.decoder)
        (Decode.field "avatar" Avatar.decoder)
        (Decode.field "name" Name.decoder)
        (Decode.field "friend_status" FriendStatus.decoder)
        (Decode.oneOf
             [ Decode.field "telegram_id" (Decode.nullable Decode.int)
             , Decode.succeed Nothing
             ])

decoderInfo : Decoder SimpleInfo
decoderInfo =
    Decode.map6 SimpleInfo
        (Decode.field "email" Email.decoder)
        (Decode.field "uid" Uid.decoder)
        (Decode.field "avatar" Avatar.decoder)
        (Decode.field "name" Name.decoder)
        (Decode.field "friend_status" FriendStatus.decoder)
        (Decode.oneOf
             [ Decode.field "telegram_id" (Decode.nullable Decode.int)
             , Decode.succeed Nothing
             ])

decoderSimpleInfo2 : Decoder SimpleInfo
decoderSimpleInfo2 =
    Decode.map6 SimpleInfo
        (at ["data", "email"] Email.decoder)
        (at ["data", "uid"] Uid.decoder)
        (at ["data", "avatar"] Avatar.decoder)
        (at ["data", "name"] Name.decoder)
        (at ["data", "friend_status"] FriendStatus.decoder)
        (Decode.oneOf
             [ at ["data", "telegram_id"] (Decode.nullable Decode.int)
             , Decode.succeed Nothing
             ])

decoderFullInfo : Decoder FullInfo
decoderFullInfo =
    Decode.succeed FullInfo
         |> Decode.map2 (|>) (Decode.field "email" Email.decoder)
         |> Decode.map2 (|>) (Decode.field "uid" Uid.decoder)
         |> Decode.map2 (|>) (Decode.field "avatar" Avatar.decoder)
         |> Decode.map2 (|>) (Decode.field "name" Name.decoder)
         |> Decode.map2 (|>) (Decode.field "friend_status" FriendStatus.decoder)
         |> Decode.map2 (|>) (Decode.field "games_count" Decode.int)
         |> Decode.map2 (|>) (Decode.field "friends_count" Decode.int)
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "friends" (Decode.list decoderInfo)))
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "incoming_friends" (Decode.list decoderInfo)))
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "outgoing_friends" (Decode.list decoderInfo)))