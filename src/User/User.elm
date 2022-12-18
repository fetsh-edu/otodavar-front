module User.User exposing (..)

import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (attribute, class, src)
import Json.Decode as Decode exposing (Decoder, at)
import Json.Encode as Encode exposing (Value)
import User.Avatar as Avatar exposing (Avatar)
import User.Bearer as Bearer exposing (Bearer)
import User.Email as Email exposing (Email)
import User.FriendStatus as FriendStatus
import User.Handle as Handle exposing (Handle)
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
    , handle : Handle
    , handleChanged : Maybe String
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
            (Decode.field "info" decoderInfo))

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
        , ( "telegram_id"
          , case uInfo.telegramId of
                Nothing -> Encode.null
                Just some -> Encode.int some
          )
        , ( "user_name", Handle.encode uInfo.handle)
        , ( "user_name_changed_at"
                  , case uInfo.handleChanged of
                        Nothing -> Encode.null
                        Just some -> Encode.string some
                  )
        ]

decoderInfo : Decoder SimpleInfo
decoderInfo =
    Decode.map8 SimpleInfo
        (Decode.field "email" Email.decoder)
        (Decode.field "uid" Uid.decoder)
        (Decode.field "avatar" Avatar.decoder)
        (Decode.field "name" Name.decoder)
        (Decode.field "friend_status" FriendStatus.decoder)
        (Decode.oneOf
             [ Decode.field "telegram_id" (Decode.nullable Decode.int)
             , Decode.succeed Nothing
             ])
        (Decode.field "user_name" Handle.decoder)
        (Decode.oneOf
             [ Decode.field "user_name_changed_at" (Decode.nullable Decode.string)
             , Decode.succeed Nothing
             ])

decoderSimpleInfo2 : Decoder SimpleInfo
decoderSimpleInfo2 =
    Decode.map8 SimpleInfo
        (at ["data", "email"] Email.decoder)
        (at ["data", "uid"] Uid.decoder)
        (at ["data", "avatar"] Avatar.decoder)
        (at ["data", "name"] Name.decoder)
        (at ["data", "friend_status"] FriendStatus.decoder)
        (Decode.oneOf
             [ at ["data", "telegram_id"] (Decode.nullable Decode.int)
             , Decode.succeed Nothing
             ])
        (at ["data", "user_name"] Handle.decoder)
        (Decode.oneOf
             [ at ["data", "user_name_changed_at"] (Decode.nullable Decode.string)
             , Decode.succeed Nothing
             ])