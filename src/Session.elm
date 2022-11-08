port module Session exposing (..)

import Browser.Navigation as Nav
import Http
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode
import Jwt exposing (decodeToken, tokenDecoder, errorToString)
import OAuth exposing (ErrorCode(..), Token)
import OAuth.Implicit as OAuth exposing (AuthorizationResultWith(..), defaultAuthorizationErrorParser, defaultErrorParser, defaultTokenParser)
import Url exposing (Url)
import Url.Parser as Url exposing ((<?>))
import Url.Parser.Query as Query
import User.User as User exposing (User)

type Session
    = LoggedIn Nav.Key Url User
    | Guest Nav.Key Url (Maybe Error)

type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo Http.Error
    | ErrCacheParse Decode.Error


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ _ ->
            key

        Guest key _ _ ->
            key

url : Session -> Url
url session =
    case session of
        LoggedIn _ url_ _ -> url_
        Guest _ url_ _ -> url_


isGuest : Session -> Bool
isGuest session =
    case session of
        LoggedIn _ _ _ -> False
        _ -> True

user : Session -> Maybe User
user session =
    case session of
        LoggedIn _ _ val ->
            Just val

        Guest _ _ _ ->
            Nothing

login : User -> Cmd msg
login user_ =
    storeSession (Just (User.encode user_))

logout : Cmd msg
logout =
    storeSession Nothing

changes : (Session -> msg) -> Nav.Key -> Url -> Sub msg
changes toMsg key url_ =
    onSessionChange (\val -> toMsg (decode key url_ val))
    --onSessionChange (\val -> toMsg (Decode.decodeValue User.decoderNullable))

decode : Nav.Key -> Url -> Decode.Value -> Session
decode key url_ value =
    case
        Decode.decodeValue User.decoderNullable value
    of
        Ok (Just decodedViewer) ->
            LoggedIn key url_ decodedViewer
        Ok Nothing ->
            Guest key url_ Nothing
        Err error ->
            Guest key url_ (Just (ErrCacheParse error))

port storeSession : Maybe Decode.Value -> Cmd msg
port onSessionChange : (Encode.Value -> msg) -> Sub msg


parseToken : Url -> AuthorizationResultWith OAuth.AuthorizationError AuthorizationSuccess
parseToken url_ =
    let
        urlPre =
            { url_ | path = "/", query = url_.fragment, fragment = Nothing }
        parseUrlQuery : a -> Query.Parser a -> a
        parseUrlQuery def parser =
            Maybe.withDefault def <| Url.parse (Url.query parser) urlPre

        googleToken : String -> Result Jwt.JwtError JwtToken
        googleToken = decodeToken googleJWT
        authorizationSuccessParser : Token -> IdToken -> Query.Parser AuthorizationSuccess
        authorizationSuccessParser accessToken idToken =
            Query.map3 (AuthorizationSuccess accessToken idToken Nothing)
                (Query.int "expires_in")
                (spaceSeparatedListParser "scope")
                (Query.string "state")
    in
    case Url.parse (Url.top <?> Query.map2 Tuple.pair defaultTokenParser defaultErrorParser) urlPre of
        Just ( Just accessToken, _ ) ->
            case Url.parse (Url.query (Query.string "id_token")) urlPre of
                Just (Just some) ->
                    case googleToken some of
                        Ok value ->
                            parseUrlQuery Empty (Query.map Success <| authorizationSuccessParser accessToken (IdToken some value))
                        Err error ->
                            Error (OAuth.AuthorizationError (Custom (errorToString error)) Nothing Nothing Nothing)
                _ -> Empty

        Just ( _, Just error ) ->
            parseUrlQuery Empty (Query.map Error <| defaultAuthorizationErrorParser error)

        _ ->
            Empty

type alias JwtToken =
        { nonce : String
        , name : String
        , picture : String
        , sub : String
        , email : String
        }


googleJWT : Decode.Decoder JwtToken
googleJWT =
    Decode.map5 JwtToken
        (field "nonce" Decode.string)
        (field "name" Decode.string)
        (field "picture" Decode.string)
        (field "sub" Decode.string)
        (field "email" Decode.string)

type alias IdToken =
    { raw : String
    , parsed : JwtToken
    }

type alias AuthorizationSuccess =
    { accessToken : Token
    , idToken : IdToken
    , refreshToken : Maybe Token
    , expiresIn : Maybe Int
    , scope : List String
    , state : Maybe String
    }


spaceSeparatedListParser : String -> Query.Parser (List String)
spaceSeparatedListParser param =
    Query.map
        (\s ->
            case s of
                Nothing ->
                    []

                Just str ->
                    String.split " " str
        )
        (Query.string param)