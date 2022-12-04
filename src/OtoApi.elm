module OtoApi exposing (..)

import Http
import RemoteData.Http exposing (defaultConfig)
import Url exposing (Protocol(..), Url)
import Url.Builder
import User.Uid as Uid exposing (Uid)


routes url__ =
    let
        url_ = url url__
    in
    { jwt = url_ ("/jwt") Nothing
    , profile = (\uid -> url_ ("/api/v1/users/" ++ (Uid.toString uid)) Nothing )
    , update_profile = url_ ("/api/v1/users/update") Nothing
    , me = url_ ("/api/v1/users/me") Nothing
    , home = url_ ("/api/v1/games") Nothing
    , friend =
        { remove = (\{uid, resource} -> url_ ("/api/v1/users/" ++ (Uid.toString uid) ++ "/unfriend") (resourceQuery resource))
        , accept = (\{uid, resource} -> url_ ("/api/v1/users/" ++ (Uid.toString uid) ++ "/accept") (resourceQuery resource))
        , request = (\{uid, resource} -> url_ ("/api/v1/users/" ++ (Uid.toString uid) ++ "/friend") (resourceQuery resource))
        }
    , notifications =
        { index = url_ ("/api/v1/notifications") Nothing
        , markAsSeen = url_ ("/api/v1/notifications/mark_as_seen") Nothing
        }
    , game =
        { start = url_ ("/api/v1/games/join") Nothing
        , show = (\uid -> url_ ("/api/v1/games/" ++ (Uid.toString uid)) Nothing )
        }
    , word =
        { create = url_ ("/api/v1/words") Nothing
        }
    , push = url_ ("/api/v1/users/push") Nothing
    }

url : Url -> String -> Maybe String -> String
url url_ path query =
    Url.toString
        { url_
        | path = path
        , query = query
        }

defaultApiUrl : Url
defaultApiUrl =
    { protocol = Https
    , host = "otodavar-api.fetsh.me"
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }

config : String -> RemoteData.Http.Config
config bearer =
    { defaultConfig
    | headers = [(Http.header "Authorization" bearer), RemoteData.Http.acceptJson]
    }

resourceQuery : Maybe Uid -> Maybe String
resourceQuery  =
    Maybe.andThen
        ( Uid.toString
        >> Url.Builder.string "resource"
        >> List.singleton
        >> Url.Builder.toQuery
        >> String.dropLeft 1
        >> Just
        )
    --Uid.toString >> (++) "resource=" >> Just