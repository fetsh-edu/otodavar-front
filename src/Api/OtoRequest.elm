module Api.OtoRequest exposing (..)

import Api.OtoApi as OtoApi
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import RemoteData exposing (WebData)
import RemoteData.Http
import User.Bearer as Bearer exposing (Bearer)



get : Maybe Bearer -> String -> (WebData success -> msg) -> Decoder success -> Cmd msg
get maybeBearer url toMsg decoder =
    case maybeBearer |> Maybe.map Bearer.toString of
        Nothing ->
            Cmd.none
        Just bearer ->
            RemoteData.Http.getWithConfig (OtoApi.config bearer) url toMsg decoder

post : Maybe Bearer -> String -> (WebData success -> msg) -> Decoder success -> Value -> Cmd msg
post maybeBearer url toMsg decoder encoder =
    case maybeBearer |> Maybe.map Bearer.toString of
        Nothing ->
            Cmd.none
        Just bearer ->
            RemoteData.Http.postWithConfig (OtoApi.config bearer) url toMsg decoder encoder