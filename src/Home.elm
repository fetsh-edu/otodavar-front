module Home exposing (..)


import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
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
            div [ class "flex flex-col m-10 justify-center items-center"]
                [ User.view user ]