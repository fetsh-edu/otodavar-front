module User.Config exposing (..)

import OAuth exposing (ResponseType(..))
import Url exposing (Protocol(..), Url)



configuration : { authorizationEndpoint : Url, clientId : String, scope : List String, responseType : ResponseType }
configuration =
    { authorizationEndpoint =
        { protocol = Https
        , port_ = Nothing
        , query = Nothing
        , fragment = Nothing
        , host = "accounts.google.com"
        , path = "/o/oauth2/v2/auth"
        }
    , clientId = "744236351761-3enuq53j5e76109de883r0uhb7cb9tc1.apps.googleusercontent.com"
    , scope = [ "profile email" ]
    , responseType = CustomResponse "id_token token"
    }
