module Main.Model exposing (..)

import About
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
    | About About.Model


getSharedModel : Model -> SharedModel
getSharedModel page =
    case page of
        Home session -> Home.toSession session
        Login model -> Login.toSession model
        Profile profile -> Profile.toSession profile
        Game game -> Game.toSession game
        ProfileEdit some ->  some.sharedModel
        About some -> some.sharedModel

updateSharedModel : SharedModel -> Model -> Model
updateSharedModel sharedModel model =
    case model of
        Home subModel ->  subModel |> Home.updateSession sharedModel |> Home
        Login subModel ->  subModel |> Login.updateSession sharedModel |> Login
        Profile subModel ->  subModel |> Profile.updateSession sharedModel |> Profile
        Game subModel ->  subModel |> Game.updateSession sharedModel |> Game
        ProfileEdit subModel -> ProfileEdit { subModel | sharedModel = sharedModel }
        About subModel -> About { subModel | sharedModel = sharedModel }