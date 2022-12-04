module Main.Model exposing (..)

import Game
import Home
import Login
import Profile
import ProfileEdit
import SharedModel exposing (SharedModel)

type Model
    = Home Home.Model
    | Login Login.Model
    | Profile Profile.Model
    | Game Game.Model
    | ProfileEdit ProfileEdit.Model


getSharedModel : Model -> SharedModel
getSharedModel page =
    case page of
        Home session -> Home.toSession session
        Login model -> Login.toSession model
        Profile profile -> Profile.toSession profile
        Game game -> Game.toSession game
        ProfileEdit some ->  some.sharedModel

updateSharedModel : SharedModel -> Model -> Model
updateSharedModel session_ model =
    case model of
        Home subModel ->  subModel |> Home.updateSession session_ |> Home
        Login subModel ->  subModel |> Login.updateSession session_ |> Login
        Profile subModel ->  subModel |> Profile.updateSession session_ |> Profile
        Game subModel ->  subModel |> Game.updateSession session_ |> Game
        ProfileEdit subModel -> ProfileEdit { subModel | sharedModel = session_ }