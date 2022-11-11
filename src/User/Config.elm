module User.Config exposing (..)

import OAuth exposing (ResponseType(..))
import Url exposing (Protocol(..), Url)



configuration : { authorizationEndpoint : Url, userInfoEndpoint : Url, clientId : String, scope : List String, responseType : ResponseType }
configuration =
    { authorizationEndpoint = { defaultHttpsUrl | host = "accounts.google.com", path = "/o/oauth2/v2/auth" }
    , userInfoEndpoint = { defaultHttpsUrl | host = "localhost", path = "/jwt", port_ = Just 3001 }
    , clientId = "744236351761-3enuq53j5e76109de883r0uhb7cb9tc1.apps.googleusercontent.com"
    , scope = [ "profile email" ]
    , responseType = CustomResponse "id_token token"
    }


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }
