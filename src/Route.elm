module Route exposing (..)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>), Parser, oneOf, s)

type Route
    = Home
    | Logout
    | Login


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Logout (s "logout")
        , Parser.map Login (s "login")
        ]

isProtected : Route -> Bool
isProtected route =
    case route of
        Home -> True
        Logout -> False
        Login -> False

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
                Logout  -> ["logout"]
                Login   -> ["login"]

    in
    "/" ++ String.join "/" pieces