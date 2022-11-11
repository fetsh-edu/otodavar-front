module Home exposing (..)


import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class)
import Route
import Session exposing (Session)
import User.User as User

type alias Model =
    { session : Session
    , game : String
    }



initModel : Session -> Model
initModel session = {session = session, game = "Game"}

init : Session -> ( Model, Cmd Msg )
init session = ( initModel session, Cmd.none )

type Msg =
    NoOp

toSession : Model -> Session
toSession model =
    model.session

view : Model -> Html Msg
view model =
    case model |> toSession |> Session.user of
        Nothing -> text ""
        Just user ->
            a [ class "flex flex-col m-10 justify-center items-center", Route.href (user |> User.info |> .uid |> Route.Profile) ] [User.view user]