module User.User exposing (..)

import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (attribute, class, src)
import Json.Decode as Decode exposing (Decoder, at)
import Json.Encode as Encode exposing (Value)
import User.Avatar as Avatar exposing (Avatar)
import User.Bearer as Bearer exposing (Bearer)
import User.Email as Email exposing (Email)
import User.Name as Name exposing (Name)
import User.Uid as Uid exposing (Uid)

type User
    = User Internals

type alias Internals =
    { bearer : Bearer
    , info : Info
    }

type alias Info =
    { email : Email
    , uid : Uid
    , avatar : Avatar
    , name : Name
    }

type Player
    = Acquaintance Info
    | Friend FullInfo

type alias FullInfo =
    { email : Email
    , uid : Uid
    , avatar : Avatar
    , name : Name
    , isFriend : Bool
    , gamesCount : Int
    , friendsCount : Int
    , friends : Maybe (List Info)
    }

build : Bearer -> Info -> User
build a b = User (Internals a b)

bearer : User -> Bearer
bearer (User intern) =
    intern.bearer

info : User -> Info
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
        , ( "info", encodeInfo intern.info )
        ]

decoder : Decoder User
decoder =
    Decode.map User
        (Decode.map2 Internals
            (Decode.field "bearer" Bearer.decoder)
            (Decode.field "info" decoderInfo))

decoderNullable : Decoder (Maybe User)
decoderNullable = Decode.nullable decoder



encodeInfo : Info -> Value
encodeInfo { email, avatar, name, uid } =
    Encode.object
        [ ( "email", Email.encode email)
        , ( "uid", Uid.encode uid)
        , ( "avatar", Avatar.encode avatar )
        , ( "name", Name.encode name )
        ]

decoderInfo : Decoder Info
decoderInfo =
    Decode.map4 Info
        (Decode.field "email" Email.decoder)
        (Decode.field "uid" Uid.decoder)
        (Decode.field "avatar" Avatar.decoder)
        (Decode.field "name" Name.decoder)

decoderInfo2 : Decoder Info
decoderInfo2 =
    Decode.map4 Info
        (at ["data", "email"] Email.decoder)
        (at ["data", "uid"] Uid.decoder)
        (at ["data", "avatar"] Avatar.decoder)
        (at ["data", "name"] Name.decoder)

decoderFullInfo : Decoder FullInfo
decoderFullInfo =
    Decode.map8 FullInfo
         (Decode.field "email" Email.decoder)
         (Decode.field "uid" Uid.decoder)
         (Decode.field "avatar" Avatar.decoder)
         (Decode.field "name" Name.decoder)
         (Decode.field "is_friend" Decode.bool)
         (Decode.field "games_count" Decode.int)
         (Decode.field "friends_count" Decode.int)
         (Decode.field "friends" (Decode.maybe (Decode.list decoderInfo)))