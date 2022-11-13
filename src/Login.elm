port module Login exposing (..)


import Base64.Encode as Base64
import Browser.Navigation as Navigation
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Dict
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Http exposing (Error(..), Expect, expectStringResponse)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode
import Jwt exposing (decodeToken, errorToString)
import OAuth exposing (ErrorCode(..), ResponseType(..), Token)
import OAuth.Implicit as OAuth exposing (AuthorizationResultWith(..), defaultAuthorizationErrorParser, defaultErrorParser, defaultTokenParser)
import Route
import Session exposing (Session, navKey)
import Url exposing (Protocol(..), Url)
import Url.Parser as Url exposing ((<?>))
import Url.Parser.Query as Query
import User.Bearer exposing (Bearer(..))
import User.Config exposing (configuration)
import User.User as User exposing (User, decoderInfo2)

type alias Model =
    { session : Session
    , flow : Flow
    }

init : Session ->  (Model, Cmd Msg)
init session =
    ( { session = session, flow = Idle}
    , if Session.isGuest session then
        Cmd.none
      else
        Navigation.pushUrl (Session.navKey session) (Route.routeToString Route.Home)
    )

type Flow
    = Idle
    | Processing
    | Erred Error

type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo Http.Error
    | ErrCacheParse Decode.Error

type Msg
    = SignInRequested
    | GotRandomBytes (List Int)
    | GotOtoUser (Result Http.Error User)
    | GotSession Session


view : Model -> { title : String, content : Html Msg }
view model =
    let
        content =
            case model.flow of
                Idle ->
                    div [ class "flex mt-32 justify-center items-center"]
                        [ signInButton ]
                Processing ->
                    div [ class "flex mt-32 justify-center items-center"]
                        [ text "LOADING" ]
                Erred error ->
                    errorView error
    in
    { title = "Login"
    , content = content
    }


errorView : Error -> Html Msg
errorView error =
    div [ class "flex m-10 justify-center items-center"]
        [ div [ class "w-full md:w-1/2 error-container on-error-container-text rounded-lg flex flex-row p-4 mb-8 text-sm shadow-md focus:outline-none focus:shadow-outline-purple" ]
            [ span [class "material-icons md-18"] [ text "error" ]
            , span [ class "ml-3 w-full overflow-ellipsis overflow-hidden" ] [ viewError error ]
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

            ErrHTTPGetUserInfo err ->
                "Unable to retrieve user info: HTTP request failed."

            ErrCacheParse error ->
                let
                    a = Debug.log "Some problems with localStorage parsing" (Decode.errorToString error)
                in
                 "Some weird unexpected error"


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


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


port genRandomBytes : Int -> Cmd msg

toSession : Model -> Session
toSession { session } = session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignInRequested ->
            ({ model | flow = Processing}, genRandomBytes 16)
        GotRandomBytes bytes ->
            gotRandomBytes { model | flow = Processing} bytes
        GotOtoUser (Ok user) ->
            ( { model | flow = Processing}, Session.login user )
        GotOtoUser (Err error) ->
            let
                a = Debug.log "A? " error
            in
            ({ model | flow = Erred (ErrHTTPGetUserInfo error)}, Cmd.none)
        GotSession session ->
            ( { model | flow = Processing, session = session }
            , Navigation.replaceUrl
                (navKey session)
                (model |> toSession |> Session.url |> cleanUrl |> Url.toString)
            )


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

----------------------------------------
--             INTERNAL               --
----------------------------------------


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



getOtoBearer : IdToken -> Cmd Msg
getOtoBearer token =
    Http.request
        { method = "POST"
        , body = Http.jsonBody (encodeToken token)
        , headers = [(Http.header "Accept" "application/json")]
        , url = Url.toString configuration.userInfoEndpoint
        , expect = expectJsonWithHeader GotOtoUser decoderInfo2 User.build
        , timeout = Nothing
        , tracker = Nothing
        }

encodeToken : IdToken -> Encode.Value
encodeToken { raw } =
    Encode.object [("jwt", Encode.string raw)]

cleanUrl : Url -> Url
cleanUrl initUrl =
    { initUrl | query = Nothing, fragment = Nothing }

rootUrl : Url -> Url
rootUrl = cleanUrl >> (\x -> {x | path = "/"})

gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    let
        { state } = convertBytes bytes
        stateUrl = model |> toSession |> Session.url |> cleanUrl |> Url.toString
        redirectUrl = model |> toSession |> Session.url |> rootUrl
        authorization =
            { clientId = configuration.clientId
            , redirectUri = redirectUrl
            , scope = configuration.scope
            , state = Just <| String.join ";" [state, stateUrl]
            , url = configuration.authorizationEndpoint
            }
    in
    ( model
    , authorization
        |> OAuth.makeAuthorizationUrlWith configuration.responseType (Dict.fromList [("nonce", state)])
        |> Url.toString
        |> Navigation.load
    )

toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


convertBytes : List Int -> { state : String }
convertBytes =
    toBytes >> base64 >> (\state -> { state = state })


expectJsonWithHeader : (Result Http.Error value -> msg) -> Decode.Decoder a -> (Bearer -> a -> value) -> Expect msg
expectJsonWithHeader toMsg decoder combiner =
  expectStringResponse toMsg <|
    \response ->
        let
            a = Debug.log "Response" response
        in
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
                        Just bearer -> Ok (combiner (Bearer bearer) value)
                        Nothing -> Err (BadBody "No Authorization token")
                Err err ->
                  Err (BadBody (Decode.errorToString err))