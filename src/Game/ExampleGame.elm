module Game.ExampleGame exposing (..)

import Delay exposing (after)
import Game.Game as Game exposing (Game, Guess(..), State)
import Game.GameStatus exposing (GameStatus(..))
import Game.Stamp as Stamp exposing (Stamp)
import Game.Word exposing (Word)
import Html
import RemoteData exposing (RemoteData(..), WebData)
import User.Avatar exposing (Avatar(..))
import User.Email exposing (Email(..))
import User.FriendStatus exposing (Status(..))
import User.Name exposing (Name(..))
import User.Uid exposing (Uid(..))

type alias Model =
    { stickerSelect : Maybe { wordId : Int }
    , guessText : String
    , guessData : WebData Game
    , game : Game
    , words : List Word
    }

initModel : Model
initModel =
    { stickerSelect = Nothing
    , guessText = ""
    , guessData = RemoteData.NotAsked
    , game = fromGame []
    , words = []
    }

initMsg : Cmd Msg
initMsg = after 800 (OnGuessChange "P")

fromGame words =
    Game.fromGame (Just (Uid "1")) (otoGame words)

fromClosedGame words =
    Game.fromGame (Just (Uid "1")) (closedGames words)

closedGames words =
    { uid = Uid ""
    , status = Closed
    , player_1 = leftPlayer
    , player_2 = Just rightPlayer
    , words = words
    }

otoGame words =
    { uid = Uid ""
    , status = Open
    , player_1 = leftPlayer
    , player_2 = Just rightPlayer
    , words = words
    }

leftPlayer =
    { email = Email ""
    , uid  = Uid "1"
    , avatar = Avatar "/images/userpic-1.jpg"
    , name = Name "Ilia"
    , friendStatus = Me
    , telegramId = Nothing
    }

rightPlayer =
    { email = Email ""
    , uid  = Uid "2"
    , avatar = Avatar "/images/userpic-2.jpg"
    , name = Name "Rami Levi"
    , friendStatus = Friend
    , telegramId = Nothing
    }

type Msg
    = PlayAgainMsg
    | OnGuessChange String
    | SubmitFirstGuess
    | AfterFirstGuess
    | GotFirstWordFromOpponent Word
    | SubmitSecondGuess
    | AfterSecondGuess
    | GotSecondWordFromOpponent Word
    | SelectSticker Int
    | SendSticker Stamp
    | SubmitThirdGuess
    | AfterThirdGuess
    | GotThirdWordFromOpponent Word
    | NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnGuessChange string ->
            let
                newMsg =
                    case string of
                        "P" -> after 300 (OnGuessChange "Pa")
                        "Pa" -> after 300 (OnGuessChange "Par")
                        "Par" -> after 200 (OnGuessChange "Parr")
                        "Parr" -> after 200 (OnGuessChange "Parro")
                        "Parro" -> after 200 (OnGuessChange "Parrot")
                        "Parrot" -> after 300 (SubmitFirstGuess)
                        "G" -> after 300 (OnGuessChange "Gr")
                        "Gr" -> after 200 (OnGuessChange "Gre")
                        "Gre" -> after 100 (OnGuessChange "Gree")
                        "Gree" -> after 300 (OnGuessChange "Green")
                        "Green" -> after 200 (SubmitSecondGuess)
                        "A" -> after 300 (OnGuessChange "Av")
                        "Av" -> after 200 (OnGuessChange "Avo")
                        "Avo" -> after 100 (OnGuessChange "Avoc")
                        "Avoc" -> after 50 (OnGuessChange "Avoca")
                        "Avoca" -> after 100 (OnGuessChange "Avocad")
                        "Avocad" -> after 200 (OnGuessChange "Avocado")
                        "Avocado" -> after 100 (SubmitThirdGuess)
                        _ -> after 1 NoOp
            in
            ( { model | guessText = string }, newMsg )
        SubmitFirstGuess ->
            ( { model | words = ({ word = model.guessText, player = leftPlayer.uid, roundId = 0, id = 0, stamp = Stamp.Nothing }) :: model.words ,guessData = Loading}
            , after 600 AfterFirstGuess
            )
        SubmitSecondGuess ->
            ( { model | words = ({ word = model.guessText, player = leftPlayer.uid, roundId = 1, id = 3, stamp = Stamp.Nothing }) :: model.words ,guessData = Loading}
            , after 300 AfterSecondGuess
            )
        AfterFirstGuess ->
            ( { model | game = fromGame model.words, guessData = NotAsked, guessText = "" }
            , after 1600 (GotFirstWordFromOpponent { word = "Cucumber", player = rightPlayer.uid, roundId = 0, id = 1, stamp = Stamp.Nothing })
            )
        AfterSecondGuess ->
            ( { model | game = fromGame model.words, guessData = NotAsked, guessText = "" }
            , after 1900 (SelectSticker 2)
            )
        GotFirstWordFromOpponent word ->
            let
                newWords = word :: model.words
            in
            ({ model | words = newWords, game = fromGame newWords}
            , after 1500 (GotSecondWordFromOpponent { word = "Tasty", player = rightPlayer.uid, roundId = 1, id = 2, stamp = Stamp.Nothing })
            )
        GotSecondWordFromOpponent word ->
            let
                newWords = word :: model.words
            in
            ({ model | words = newWords, game = fromGame newWords}
            , after 900 (OnGuessChange "G")
            )
        SelectSticker int ->
            ( { model | stickerSelect = Just { wordId = int } }
            , after 2000 (SendSticker Stamp.Omg)
            )
        SendSticker stamp ->
            ( { model
              | stickerSelect = Nothing
              , words = List.map (\x -> if x.id == 2 then { word = "Tasty", player = rightPlayer.uid, roundId = 1, id = 2, stamp = stamp } else x ) model.words
              , game = Game.updateWord { word = "Tasty", player = rightPlayer.uid, roundId = 1, id = 2, stamp = stamp } model.game
              }
            , Cmd.batch
                [ after 900 (OnGuessChange "A")
                , after 1300 (GotThirdWordFromOpponent { word = "Avocado", player = rightPlayer.uid, roundId = 2, id = 4, stamp = Stamp.Nothing })
                ]
            )
        GotThirdWordFromOpponent word ->
            let
                newWords = word :: model.words
            in
            ({ model | words = newWords, game = fromGame newWords}
            , Cmd.none
            )
        SubmitThirdGuess ->
            ( { model | words = ({ word = model.guessText, player = leftPlayer.uid, roundId = 2, id = 5, stamp = Stamp.Nothing }) :: model.words ,guessData = Loading}
            , after 300 AfterThirdGuess
            )
        AfterThirdGuess ->
            ( { model | game = fromClosedGame model.words, guessData = NotAsked, guessText = "" }
            , Cmd.none
            )
        PlayAgainMsg -> (initModel, after 0 (OnGuessChange "P"))
        NoOp -> (model, Cmd.none)


view toSelf model =
    let
        translator =
            { playAgainMsg = always PlayAgainMsg >> toSelf
            , closeStickersMsg = toSelf NoOp
            , createStampMsg = SelectSticker >> toSelf
            , stampSelectMsg = SendSticker >> toSelf
            , submitGuessMsg = toSelf NoOp
            , onGuessChangeMsg = OnGuessChange >> toSelf
            }
    in
    case model.game of
       Game.WrongState _ -> Html.text ""
       Game.RightState state ->
           Html.div [] (Game.gameView translator model.stickerSelect model.guessText model.guessData state False)



