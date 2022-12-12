module Game.ComposedStatus exposing (..)

type ComposedStatus
    = Archived
    | Finished
    | MyTurn
    | PartnersTurn
    | WrongStatus
    | OthersGame

toString : ComposedStatus -> String
toString status =
    case status of
        Archived -> "archived"
        Finished -> "finished"
        MyTurn -> "my_turn"
        PartnersTurn -> "partners_turn"
        OthersGame -> "others_game"
        WrongStatus -> "wrong_status"

fromString : String -> ComposedStatus
fromString string =
    case string of
         "archived" -> Archived
         "finished" -> Finished
         "my_turn" -> MyTurn
         "partners_turn" -> PartnersTurn
         "others_game" -> OthersGame
         "wrong_status" -> WrongStatus
         _ -> WrongStatus