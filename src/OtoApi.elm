module OtoApi exposing (..)

import Http
import RemoteData.Http exposing (defaultConfig)
import Url
import Url.Builder
import User.Config exposing (defaultHttpsUrl)
import User.Uid as Uid exposing (Uid)


routes =
    { friend =
        { remove = (\{uid, resource} -> url ("/api/v1/users/" ++ (Uid.toString uid) ++ "/unfriend") (resourceQuery resource))
        , accept = (\{uid, resource} -> url ("/api/v1/users/" ++ (Uid.toString uid) ++ "/accept") (resourceQuery resource))
        , request = (\{uid, resource} -> url ("/api/v1/users/" ++ (Uid.toString uid) ++ "/friend") (resourceQuery resource))
        }
    }

url : String -> Maybe String -> String
url path query =
    Url.toString
        { defaultHttpsUrl
        | host = "localhost"
        , path = path
        , port_ = Just 3001
        , query = query
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