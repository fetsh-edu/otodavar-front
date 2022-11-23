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
    , info : UserInfo
    }

type alias UserInfo =
    { email : Email
    , uid : Uid
    , avatar : Avatar
    , name : Name
    , friends : List SimpleInfo
    , incomingFriendRequests : List SimpleInfo
    , outgoingFriendRequests : List SimpleInfo
    }

type alias SimpleInfo =
    { email : Email
    , uid : Uid
    , avatar : Avatar
    , name : Name
    }

userToSimple : UserInfo -> SimpleInfo
userToSimple user =
    { email = user.email
    , uid = user.uid
    , avatar = user.avatar
    , name = user.name
    }


type alias FullInfo =
    { email : Email
    , uid : Uid
    , avatar : Avatar
    , name : Name
    , gamesCount : Int
    , friendsCount : Int
    , friends : Maybe (List SimpleInfo)
    , incomingFriendRequests : Maybe (List SimpleInfo)
    , outgoingFriendRequests : Maybe (List SimpleInfo)
    }


friendStatus : User -> Uid -> FriendStatus.Status
friendStatus user uid =
    if (user |> info |> .uid) == uid then
        FriendStatus.Me
    else if (user |> info |> .friends |> List.any (\x -> x.uid == uid) ) then
        FriendStatus.Friend
    else if (user |> info |> .incomingFriendRequests |> List.any (\x -> x.uid == uid) ) then
        FriendStatus.Wannabe
    else if (user |> info |> .outgoingFriendRequests |> List.any (\x -> x.uid == uid) ) then
        FriendStatus.Requested
    else
        FriendStatus.Unknown

build : Bearer -> UserInfo -> User
build a b = User (Internals a b)

bearer : User -> Bearer
bearer (User intern) =
    intern.bearer

info : User -> UserInfo
info (User intern) =
    intern.info

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

encodeUserInfo : UserInfo -> Value
encodeUserInfo uInfo =
    Encode.object
        [ ( "email", Email.encode uInfo.email)
        , ( "uid", Uid.encode uInfo.uid)
        , ( "avatar", Avatar.encode uInfo.avatar )
        , ( "name", Name.encode uInfo.name )
        , ( "friends", (Encode.list encodeInfo uInfo.friends) )
        , ( "incoming_friends", (Encode.list encodeInfo uInfo.incomingFriendRequests) )
        , ( "outgoing_friends", (Encode.list encodeInfo uInfo.outgoingFriendRequests) )
        ]

encodeInfo : SimpleInfo -> Value
encodeInfo { email, avatar, name, uid } =
    Encode.object
        [ ( "email", Email.encode email)
        , ( "uid", Uid.encode uid)
        , ( "avatar", Avatar.encode avatar )
        , ( "name", Name.encode name )
        ]

decoderUserInfo : Decoder UserInfo
decoderUserInfo =
    Decode.map7 UserInfo
        (Decode.field "email" Email.decoder)
        (Decode.field "uid" Uid.decoder)
        (Decode.field "avatar" Avatar.decoder)
        (Decode.field "name" Name.decoder)
        (Decode.field "friends" (Decode.list decoderInfo))
        (Decode.field "incoming_friends" (Decode.list decoderInfo))
        (Decode.field "outgoing_friends" (Decode.list decoderInfo))

decoderInfo : Decoder SimpleInfo
decoderInfo =
    Decode.map4 SimpleInfo
        (Decode.field "email" Email.decoder)
        (Decode.field "uid" Uid.decoder)
        (Decode.field "avatar" Avatar.decoder)
        (Decode.field "name" Name.decoder)


-- TODO: Check
decoderUserInfo2 : Decoder UserInfo
decoderUserInfo2 =
    Decode.map7 UserInfo
        (at ["data", "email"] Email.decoder)
        (at ["data", "uid"] Uid.decoder)
        (at ["data", "avatar"] Avatar.decoder)
        (at ["data", "name"] Name.decoder)
        (at ["data", "friends"] (Decode.list decoderInfo))
        (at ["data", "incoming_friends"] (Decode.list decoderInfo))
        (at ["data", "outgoing_friends"] (Decode.list decoderInfo))

decoderFullInfo : Decoder FullInfo
decoderFullInfo =
    Decode.succeed FullInfo
         |> Decode.map2 (|>) (Decode.field "email" Email.decoder)
         |> Decode.map2 (|>) (Decode.field "uid" Uid.decoder)
         |> Decode.map2 (|>) (Decode.field "avatar" Avatar.decoder)
         |> Decode.map2 (|>) (Decode.field "name" Name.decoder)
         |> Decode.map2 (|>) (Decode.field "games_count" Decode.int)
         |> Decode.map2 (|>) (Decode.field "friends_count" Decode.int)
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "friends" (Decode.list decoderInfo)))
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "incoming_friends" (Decode.list decoderInfo)))
         |> Decode.map2 (|>) (Decode.maybe (Decode.field "outgoing_friends" (Decode.list decoderInfo)))