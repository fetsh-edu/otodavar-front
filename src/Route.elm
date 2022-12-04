module Route exposing (..)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import User.Uid as Uid exposing (Uid(..))

type Route
    = Home
    | Profile Uid
    | ProfileEdit
    | Game Uid
    | Login


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map ProfileEdit (s "profile")
        , Parser.map Profile (s "u" </> Parser.map Uid Parser.string)
        , Parser.map Game (s "g" </> Parser.map Uid Parser.string)
        , Parser.map Login (s "login")
        ]


isProtected : Route -> Bool
isProtected route =
    case route of
        Home -> True
        Profile _ -> True
        Game _ -> True
        ProfileEdit -> True
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
                Profile uid -> [ "u", Uid.toString uid ]
                Game uid -> [ "g", Uid.toString uid ]
                Login   -> [ "login" ]
                ProfileEdit -> [ "profile" ]

    in
    "/" ++ String.join "/" pieces