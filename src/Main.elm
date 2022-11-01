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
import Http exposing (Error(..))
import Json.Decode as Decode exposing (field)
import Http
import Json.Decode as Json
import Jwt exposing (decodeToken, tokenDecoder, errorToString)
import Maybe exposing (withDefault)
import OAuth exposing (ErrorCode(..), ResponseType(..), Token, errorCodeFromString, makeToken, tokenToString)
import OAuth.Implicit as OAuth exposing (AuthorizationResultWith(..), defaultAuthorizationErrorParser, defaultErrorParser, defaultTokenParser, parseTokenWith)
import Url exposing (Protocol(..), Url)
import Url.Parser as Url exposing ((<?>))
import Url.Parser.Query as Query

-- ---------------------------
-- PORTS
-- ---------------------------

configuration : Configuration
configuration =
    { authorizationEndpoint =
        { defaultHttpsUrl | host = "accounts.google.com", path = "/o/oauth2/v2/auth" }
    , userInfoEndpoint =
        { defaultHttpsUrl | host = "www.googleapis.com", path = "/oauth2/v1/userinfo" }
    , clientId = "744236351761-3enuq53j5e76109de883r0uhb7cb9tc1.apps.googleusercontent.com"
    , scope = [ "profile email" ]
    , responseType = CustomResponse "id_token token"
    }


-- ---------------------------
-- MODEL
-- ---------------------------

type alias Model =
    { redirectUri : Url
    , flow : Flow
    }

type Flow
    = Idle
    | Authorized OAuth.Token IdToken
    | Errored Error

type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo

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
init : Maybe { state : String } -> Url -> Key -> ( Model, Cmd Msg )
init mflags origin navigationKey =
    let
        do = Debug.log "Origin: " origin
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }

        clearUrl =
            Navigation.replaceUrl navigationKey (Url.toString redirectUri)
    in
    case parseToken origin of
        OAuth.Empty ->
            ( { flow = Idle, redirectUri = redirectUri }
            , Cmd.none
            )

        -- It is important to set a `state` when making the authorization request
        -- and to verify it after the redirection. The state can be anything but its primary
        -- usage is to prevent cross-site request forgery; at minima, it should be a short,
        -- non-guessable string, generated on the fly.
        --
        -- We remember any previously generated state  state using the browser's local storage
        -- and give it back (if present) to the elm application upon start
        OAuth.Success { accessToken, state, idToken } ->
            case mflags of
                Nothing ->
                    ( { flow = Errored ErrStateMismatch, redirectUri = redirectUri }
                    , clearUrl
                    )

                Just flags ->
                    if state /= Just flags.state || idToken.parsed.nonce /= flags.state then
                        ( { flow = Errored ErrStateMismatch, redirectUri = redirectUri }
                        , clearUrl
                        )

                    else
                        ( { flow = Authorized accessToken idToken, redirectUri = redirectUri }
                        , clearUrl
                        )

        OAuth.Error error ->
            ( { flow = Errored <| ErrAuthorization error, redirectUri = redirectUri }
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
    | SignOutRequested

--getUserInfo : Configuration -> OAuth.Token -> Cmd Msg
--getUserInfo { userInfoDecoder, userInfoEndpoint } token =
--    Http.request
--        { method = "GET"
--        , body = Http.emptyBody
--        , headers = OAuth.useToken token []
--        , url = Url.toString userInfoEndpoint
--        , expect = Http.expectJson GotUserInfo userInfoDecoder
--        , timeout = Nothing
--        , tracker = Nothing
--        }


{- On the JavaScript's side, we have:
   app.ports.genRandomBytes.subscribe(n => {
     const buffer = new Uint8Array(n);
     crypto.getRandomValues(buffer);
     const bytes = Array.from(buffer);
     localStorage.setItem("bytes", bytes);
     app.ports.randomBytes.send(bytes);
   });
-}


port genRandomBytes : Int -> Cmd msg

port randomBytes : (List Int -> msg) -> Sub msg

--
-- Update
--

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.flow, msg ) of
        ( Idle, SignInRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Authorized token mbIdToken, SignOutRequested ) ->
            signOutRequested model

        _ ->
            noOp model


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )


signInRequested : Model -> ( Model, Cmd Msg )
signInRequested model =
    ( { model | flow = Idle }
    , genRandomBytes 16
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    let
        { state } =
            convertBytes bytes

        authorization =
            { clientId = configuration.clientId
            , redirectUri = model.redirectUri
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
    ( { model | flow = Idle }
    , Navigation.load (Url.toString model.redirectUri)
    )


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
        Authorized token mbIdToken ->
            div [][ div [] [ text (tokenToString token)]
            , div [] [ text (mbIdToken.parsed.email)]
            , div [] [ button
                        [ onClick SignOutRequested, class "" ]
                        [ text "Sign out" ]
                    ]
            ]
        Errored error -> div [ class "flex" ]
                             [ div
                                       [ class "step", class "step-errored" ]
                                       [ span [ style "left" "-50%" ] [ text "Errored" ]
                                       , span [ class "span-error" ] [ viewError error ] ]
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

-- ---------------------------
-- MAIN
-- ---------------------------


main : Program (Maybe (List Int)) Model Msg
main =
    application
        { init = Maybe.map convertBytes >> init
        , update = update
        , subscriptions = always <| randomBytes GotRandomBytes
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        , view = view
        }


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