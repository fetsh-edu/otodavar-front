module About exposing (..)

import Browser exposing (Document)
import Game.ExampleGame as ExampleGame
import Html exposing (Html)
import SharedModel exposing (SharedModel)
import View.Helper


type alias Model =
    { sharedModel : SharedModel
    , example : ExampleGame.Model
    }

initModel : SharedModel -> Model
initModel sharedModel =
    { sharedModel = sharedModel, example = ExampleGame.initModel}

init : SharedModel -> (Model, Cmd ExampleGame.Msg)
init sharedModel =
    (initModel sharedModel, ExampleGame.initMsg )

view : { toSelf : ExampleGame.Msg -> msg } -> Model -> Document msg
view translator model =
    { title = "About"
    , body = [ View.Helper.container <| View.Helper.about (game translator model) ]
    }

game : { toSelf : ExampleGame.Msg -> msg } -> Model -> Html msg
game translator model = ExampleGame.view translator.toSelf model.example