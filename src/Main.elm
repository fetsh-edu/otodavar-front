port module Main exposing (Model, Msg(..), init, main, update, view)

import Base64.Encode as Base64
import Browser exposing (Document, application)
import Browser.Navigation as Navigation exposing (Key)
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Delay exposing (Millis, after)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error(..), Expect, expectStringResponse)
import Json.Decode as Decode exposing (at, decodeString, field)
import Json.Encode as Encode
import Http
import Json.Decode as Json
import Jwt exposing (decodeToken, tokenDecoder, errorToString)
import Maybe exposing (withDefault)
import OAuth exposing (ErrorCode(..), ResponseType(..), Token, errorCodeFromString, makeToken, tokenToString)
import OAuth.Implicit as OAuth exposing (AuthorizationResultWith(..), defaultAuthorizationErrorParser, defaultErrorParser, defaultTokenParser, parseTokenWith)
import Url exposing (Protocol(..), Url)
import Url.Parser as Url exposing ((<?>))
import Url.Parser.Query as Query


configuration : Configuration
configuration =
    { authorizationEndpoint = { defaultHttpsUrl | host = "accounts.google.com", path = "/o/oauth2/v2/auth" }
    , userInfoEndpoint = { defaultHttpsUrl | host = "localhost", path = "/jwt", port_ = Just 3001 }
    , clientId = "744236351761-3enuq53j5e76109de883r0uhb7cb9tc1.apps.googleusercontent.com"
    --, scope = [ "profile" ]
    , scope = [ "profile email" ]
    , responseType = CustomResponse "id_token token"
    --, responseType = CustomResponse "id_token token"
    }

main : Program Flags Model Msg
main =
    application
        { init = init
        , update = update
        , subscriptions = always <| Sub.batch
            [ randomBytes GotRandomBytes
            , onSessionChange decodeFromStorage
            ]
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        , view = view
        }



userFromStorageDecoder : Decode.Decoder (Maybe OtoUser)
userFromStorageDecoder = Decode.oneOf
    [ Decode.map Just decodeUser
    , Decode.null Nothing
    ]

decodeFromStorage : Decode.Value -> Msg
decodeFromStorage =
    GotOtoUserFromStorage << Decode.decodeValue userFromStorageDecoder


-- ---------------------------
-- MODEL
-- ---------------------------

type alias Model =
    { rootUrl : Url
    , flow : Flow
    }

type Flow
    = Idle
    | GoogleAuthed OAuth.Token IdToken
    | OtoAuthed OtoUser
    | Erred Error

type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo
    | ErrCacheParse Decode.Error

type alias Configuration =
    { authorizationEndpoint : Url
    , userInfoEndpoint : Url
    , clientId : String
    , scope : List String
    , responseType : ResponseType
    }

{-| During the authentication flow, we'll run twice into the `init` function:
  - The first time, for the application very first run. And we proceed with the `Idle` state,
    waiting for the user (a.k.a you) to request a sign in.
  - The second time, after a sign in has been requested, the user is redirected to the
    authorization server and redirects the user back to our application, with an access
    token and other fields as query parameters.
When query params are present (and valid), we consider the user `Authorized`.
-}

type alias Flags =
  { bytes : Maybe (List Int)
  , bearer : Encode.Value
  }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags origin navigationKey =
    let
        maybeBytes = flags.bytes |> Maybe.map convertBytes
        maybeBearer = Debug.log "Initial Bearer" flags.bearer
        redirectUri = { origin | query = Nothing, fragment = Nothing }
        clearUrl = Navigation.replaceUrl navigationKey (Url.toString redirectUri)
    in
    case parseToken origin of
        OAuth.Empty ->
            withStorageToken
                { flow = Idle, rootUrl = redirectUri }
                ((Decode.decodeValue userFromStorageDecoder maybeBearer))

        OAuth.Success { accessToken, state, idToken } ->
            case maybeBytes of
                Nothing ->
                    ( { flow = Erred ErrStateMismatch, rootUrl = redirectUri }
                    , clearUrl
                    )

                Just bytes ->
                    if state /= Just bytes.state || idToken.parsed.nonce /= bytes.state then
                        ( { flow = Erred ErrStateMismatch, rootUrl = redirectUri }
                        , clearUrl
                        )

                    else
                        ( { flow = GoogleAuthed accessToken idToken, rootUrl = redirectUri }
                        , Cmd.batch
                            [ after 500 RequestOtoBearer
                            , clearUrl
                            ]
                        )

        OAuth.Error error ->
            ( { flow = Erred <| ErrAuthorization error, rootUrl = redirectUri }
            , clearUrl
            )

parseToken : Url -> AuthorizationResultWith OAuth.AuthorizationError AuthorizationSuccess
parseToken url_ =
    let
        url =
            { url_ | path = "/", query = url_.fragment, fragment = Nothing }
        parseUrlQuery : a -> Query.Parser a -> a
        parseUrlQuery def parser =
            Maybe.withDefault def <| Url.parse (Url.query parser) url

        googleToken : String -> Result Jwt.JwtError JwtToken
        googleToken = decodeToken googleJWT
        authorizationSuccessParser : Token -> IdToken -> Query.Parser AuthorizationSuccess
        authorizationSuccessParser accessToken idToken =
            Query.map3 (AuthorizationSuccess accessToken idToken Nothing)
                (Query.int "expires_in")
                (spaceSeparatedListParser "scope")
                (Query.string "state")
    in
    case Url.parse (Url.top <?> Query.map2 Tuple.pair defaultTokenParser defaultErrorParser) url of
        Just ( Just accessToken, _ ) ->
            case Url.parse (Url.query (Query.string "id_token")) url of
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
    Decode.succeed JwtToken
        |> Decode.map2 (|>) (field "nonce" Decode.string)
        |> Decode.map2 (|>) (field "name" Decode.string)
        |> Decode.map2 (|>) (field "picture" Decode.string)
        |> Decode.map2 (|>) (field "sub" Decode.string)
        |> Decode.map2 (|>) (field "email" Decode.string)

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


-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = NoOp
    | SignInRequested
    | GotRandomBytes (List Int)
    | RequestOtoBearer
    | GotOtoUser (Result Http.Error OtoUser)
    | GotOtoUserFromStorage (Result Decode.Error (Maybe OtoUser))
    | SignOutRequested

port genRandomBytes : Int -> Cmd msg

port randomBytes : (List Int -> msg) -> Sub msg

port storeSession : Maybe Decode.Value -> Cmd msg

port onSessionChange : (Encode.Value -> msg) -> Sub msg

--
-- Update
--

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.flow, msg ) of
        (_, NoOp) ->
            noOp model

        ( _, GotOtoUser userinfo ) ->
            let
                deb = Debug.log "GotOtoUser" userinfo
            in
                case userinfo of
                    Ok user ->
                        ( model, storeSession (Just (encodeUser user)) )
                    Err error ->
                        ( { model | flow = Erred ErrHTTPGetUserInfo}, Cmd.none )

        ( _, GotOtoUserFromStorage resultMaybeOtoUser ) ->
            withStorageToken model resultMaybeOtoUser

        ( Idle, SignInRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Idle, _ ) -> noOp model
        ( GoogleAuthed _ _, SignInRequested ) -> noOp model
        ( GoogleAuthed _ _, GotRandomBytes _ ) -> noOp model

        ( GoogleAuthed accessToken idToken, RequestOtoBearer ) ->
            requestOtoBearer model accessToken idToken

        (_, SignOutRequested) ->
            signOutRequested model
        ( OtoAuthed _, _ ) ->
            noOp model
        ( Erred _, _) ->
            noOp model

withStorageToken : Model -> Result Decode.Error (Maybe OtoUser) -> ( Model, Cmd Msg )
withStorageToken model resultMaybeOtoUser =
    case resultMaybeOtoUser of
        Ok maybeOtoUser ->
            case maybeOtoUser of
                Nothing ->
                    ( { model | flow = Idle }, Cmd.none )
                Just user ->
                    ( { model | flow = OtoAuthed user }, Cmd.none )

        Err error ->
            ( { model | flow = Erred (ErrCacheParse error) }, Cmd.none )

noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )


signInRequested : Model -> ( Model, Cmd Msg )
signInRequested model =
    ( { model | flow = Idle }
    , genRandomBytes 16
    )

requestOtoBearer : Model -> OAuth.Token -> IdToken -> ( Model, Cmd Msg )
requestOtoBearer model accessToken idToken =
    ( { model | flow = GoogleAuthed accessToken idToken }
    , getOtoBearer idToken
    )

getOtoBearer : IdToken -> Cmd Msg
getOtoBearer token =
    Http.request
        { method = "POST"
        , body = Http.jsonBody (encodeToken token)
        , headers = [(Http.header "Accept" "application/json")]
        , url = Url.toString configuration.userInfoEndpoint
        , expect = expectJsonWithHeader GotOtoUser userInfoDecoder OtoUser
        , timeout = Nothing
        , tracker = Nothing
        }


expectJsonWithHeader : (Result Http.Error value -> msg) -> Decode.Decoder a -> (String -> a -> value) -> Expect msg
expectJsonWithHeader toMsg decoder combiner =
  expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (Http.BadUrl url)

        Http.Timeout_ ->
          Err Http.Timeout

        Http.NetworkError_ ->
          Err Http.NetworkError

        Http.BadStatus_ metadata body ->
          Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ metadata body ->
          case Decode.decodeString decoder body of
            Ok value ->
                case Dict.get "authorization" metadata.headers of
                    Just bearer -> Ok (combiner bearer value)
                    Nothing -> Err (BadBody "No Authorization token")
            Err err ->
              Err (BadBody (Decode.errorToString err))



type alias OtoUser =
    { bearer : String
    , info : OtoUserInfo
    }

type alias OtoUserInfo =
    { email : String
    , uid : String
    , avatar : String
    , name : String
    }

decodeUser : Decode.Decoder OtoUser
decodeUser =
    Decode.map2 OtoUser
        (field "bearer" Decode.string)
        (field "info" decodeUserInfo)

decodeUserInfo : Decode.Decoder OtoUserInfo
decodeUserInfo =
    Decode.map4 OtoUserInfo
        (field "email" Decode.string)
        (field "uid" Decode.string)
        (field "avatar" Decode.string)
        (field "name" Decode.string)

encodeUser : OtoUser -> Encode.Value
encodeUser user =
    Encode.object
        [ ("bearer", Encode.string user.bearer)
        , ("info", encodeUserInfo user.info)
        ]

encodeUserInfo : OtoUserInfo -> Encode.Value
encodeUserInfo userInfo =
    Encode.object
        [ ("email", Encode.string userInfo.email)
        , ("uid", Encode.string userInfo.uid)
        , ("avatar", Encode.string userInfo.avatar)
        , ("name", Encode.string userInfo.name)
        ]

encodeToken : IdToken -> Encode.Value
encodeToken { raw } =
    Encode.object [("jwt", Encode.string raw)]

userInfoDecoder : Decode.Decoder OtoUserInfo
userInfoDecoder =
        Decode.map4 OtoUserInfo
            (at ["data", "email"] Decode.string)
            (at ["data", "uid"] Decode.string)
            (at ["data", "avatar"] Decode.string)
            (at ["data", "name"] Decode.string)

gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    let
        { state } =
            convertBytes bytes

        authorization =
            { clientId = configuration.clientId
            , redirectUri = model.rootUrl
            , scope = configuration.scope
            , state = Just state
            , url = configuration.authorizationEndpoint
            }
    in
    ( { model | flow = Idle }
    , authorization
        |> OAuth.makeAuthorizationUrlWith configuration.responseType (Dict.fromList [("nonce", state)])
        |> Url.toString
        |> Navigation.load
    )

signOutRequested : Model -> ( Model, Cmd Msg )
signOutRequested model =
    ( { model | flow = Idle }, storeSession Nothing )


-- ---------------------------
-- VIEW
-- ---------------------------


viewBody : Model -> Html Msg
viewBody model =
    main_ [class "surface on-surface-text pt-10", style "height" "100%", style "color-scheme" "dark"]
        [ header_
        , gameView model
        ]


header_ : Html Msg
header_ =
    header [class "flex flex-col"]
        [ div [ class "flex flex-row justify-center" ]
            [ span [ class "tertiary-container on-tertiary-container-text letter"] [ text "o" ]
            , span [ class "-ml-1.5 tertiary-container on-tertiary-container-text  letter"] [ text "t" ]
            , span [ class "-ml-1.5 tertiary-container on-tertiary-container-text  letter"] [ text "o" ]
            , span [ class "secondary-container on-secondary-container-text letter"] [ text "d" ]
            , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "a" ]
            , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "v" ]
            , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "a" ]
            , span [ class "-ml-1.5 secondary-container on-secondary-container-text letter"] [ text "r" ]
            ]
        , div [ class "text-s pt-2 justify-center"] [ text "The game you've been waiting for so long"]
        ]

gameView : Model -> Html Msg
gameView model =
    case model.flow of
        Idle -> signInView
        Erred error -> errorView error
        _ -> authorizedView model


authorizedView : Model -> Html Msg
authorizedView model =
    div [ class "flex flex-col m-10 justify-center items-center"]
        [ googleCredsView model.flow
        , div [] [ button
                    [ onClick SignOutRequested, class "" ]
                    [ text "Sign out" ]
                ]
        ]


googleCredsView : Flow -> Html msg
googleCredsView flow =
    let
        img_ =
            case flow of
                GoogleAuthed _ idToken -> img [ attribute "referrerpolicy" "no-referrer", class "w-14 h-14 rounded-full", src (idToken.parsed.picture)] []
                OtoAuthed otoUser -> img [ attribute "referrerpolicy" "no-referrer", class "w-14 h-14 rounded-full", src (otoUser.info.avatar)] []
                _ -> text ""
        info_ =
            case flow of
                GoogleAuthed _ idToken -> span [ class "pl-4 overflow-ellipsis overflow-hidden"] [text (idToken.parsed.email)]
                OtoAuthed otoUser ->  span [ class "pl-4 overflow-ellipsis overflow-hidden"] [text (otoUser.info.name)]
                _ -> text ""
    in
        div [ class "transition-transform transform  w-full md:w-1/2 surface-1 on-surface-text rounded-lg flex flex-row p-4 mb-8 text-lg shadow-md justify-left items-center" ]
            [ img_
            , info_
            ]


errorView : Error -> Html Msg
errorView error =
    div [ class "flex m-10 justify-center items-center"]
        [ div [ class "w-full md:w-1/2 error-container on-error-container-text rounded-lg flex flex-row p-4 mb-8 text-sm shadow-md focus:outline-none focus:shadow-outline-purple" ]
            [ span [class "material-icons md-18"] [ text "error" ]
            , span [ class "ml-3 w-full overflow-ellipsis overflow-hidden" ] [ viewError error ]
            ]
        ]

signInView : Html Msg
signInView =
    div [ class "flex mt-32 justify-center items-center"]
        [ signInButton ]

signInButton : Html Msg
signInButton =
    div [ style "height" "50px", style "width" "240px", class "abcRioButton abcRioButtonBlue", onClick SignInRequested]
        [ div [ class "abcRioButtonContentWrapper"]
            [ div [ class "abcRioButtonIcon", style "padding" "15px"]
                [ div [ style "width" "18px", style "height" "18px", class "abcRioButtonSvgImageWithFallback abcRioButtonIconImage abcRioButtonIconImage18"]
                    [ img [src "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='18px' height='18px' viewBox='0 0 48 48' class='abcRioButtonSvg'%3E%3Cg%3E%3Cpath fill='%23EA4335' d='M24 9.5c3.54 0 6.71 1.22 9.21 3.6l6.85-6.85C35.9 2.38 30.47 0 24 0 14.62 0 6.51 5.38 2.56 13.22l7.98 6.19C12.43 13.72 17.74 9.5 24 9.5z'%3E%3C/path%3E%3Cpath fill='%234285F4' d='M46.98 24.55c0-1.57-.15-3.09-.38-4.55H24v9.02h12.94c-.58 2.96-2.26 5.48-4.78 7.18l7.73 6c4.51-4.18 7.09-10.36 7.09-17.65z'%3E%3C/path%3E%3Cpath fill='%23FBBC05' d='M10.53 28.59c-.48-1.45-.76-2.99-.76-4.59s.27-3.14.76-4.59l-7.98-6.19C.92 16.46 0 20.12 0 24c0 3.88.92 7.54 2.56 10.78l7.97-6.19z'%3E%3C/path%3E%3Cpath fill='%2334A853' d='M24 48c6.48 0 11.93-2.13 15.89-5.81l-7.73-6c-2.15 1.45-4.92 2.3-8.16 2.3-6.26 0-11.57-4.22-13.47-9.91l-7.98 6.19C6.51 42.62 14.62 48 24 48z'%3E%3C/path%3E%3Cpath fill='none' d='M0 0h48v48H0z'%3E%3C/path%3E%3C/g%3E%3C/svg%3E"][] ]
                ]
            , span [ style "font-size" "16px", style "line-height" "48px", class "abcRioButtonContents"][ text "Sign in with Google"]
            ]
        ]


viewError : Error -> Html Msg
viewError e =
    text <|
        case e of
            ErrStateMismatch ->
                "'state' doesn't match, the request has likely been forged by an adversary!"

            ErrAuthorization error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            ErrHTTPGetUserInfo ->
                "Unable to retrieve user info: HTTP request failed."

            ErrCacheParse error ->
                let
                    a = Debug.log "Some problems with localStorage parsing" (Decode.errorToString error)
                in
                 "Some weird unexpected error"



view : Model -> Document Msg
view  model =
    { title = "oto|davar"
    , body = [viewBody model]
    }


--
-- Helpers
--

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

toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


convertBytes : List Int -> { state : String }
convertBytes =
    toBytes >> base64 >> (\state -> { state = state })


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }

defaultHttpUrl : Url
defaultHttpUrl =
    { protocol = Http
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }