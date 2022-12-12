module Game.GameStatus exposing (..)

import Json.Decode as Decode exposing (Decoder)
type GameStatus
    = Open
    | Closed

decoder : Decoder GameStatus
decoder =
    Decode.string
        |> Decode.andThen (\x ->
            case x of
                "open" -> Decode.succeed Open
                "closed" -> Decode.succeed Closed
                _ -> Decode.fail ("Bad game status " ++ x)
        )

toString : GameStatus -> String
toString status =
    case status of
        Open -> "open"
        _ -> "closed"
