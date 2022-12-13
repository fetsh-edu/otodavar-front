module Route exposing (..)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import User.Handle as Handle exposing (Handle(..))
import User.Uid as Uid exposing (Uid(..))

type Route
    = Home
    | Profile Handle
    | ProfileEdit
    | Game Uid
    | Login
    | About


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map ProfileEdit (s "profile")
        , Parser.map Profile (s "u" </> Parser.map Handle Parser.string)
        , Parser.map Game (s "g" </> Parser.map Uid Parser.string)
        , Parser.map Login (s "login")
        , Parser.map About (s "about")
        ]


isProtected : Route -> Bool
isProtected route =
    case route of
        Login -> False
        Home -> True
        Profile _ -> True
        Game _ -> True
        ProfileEdit -> True
        About -> True


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


fromUrl : Url -> Maybe Route
fromUrl url = Parser.parse parser url

replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home    -> []
                Profile uid -> [ "u", Handle.toString uid ]
                Game uid -> [ "g", Uid.toString uid ]
                Login   -> [ "login" ]
                ProfileEdit -> [ "profile" ]
                About -> ["about"]


    in
    "/" ++ String.join "/" pieces