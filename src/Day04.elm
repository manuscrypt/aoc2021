module Day04 exposing (..)

import Dict
import Dict.Extra as Dict
import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type alias Cell =
    { x : Int
    , y : Int
    , val : Int
    , marked : Bool
    }


type alias Board =
    { cells : List Cell
    , winnerIdx : Maybe Int
    , winNum : Maybe Int
    }


type alias Model =
    { numbers : List Int
    , boards : List Board
    }


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d04" "input"

        sample =
            getInput "d04" "sample"
    in
    IO.do
        (IO.combine
            [ sample
                |> IO.map (parse >> solveA)
                |> IO.andThen (output "Sample Part A: ")
            , sample
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Sample Part B: ")
            , input
                |> IO.map (parse >> solveA)
                |> IO.andThen (output "Part A: ")
            , input
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


solveA : Model -> Int
solveA model =
    winnerBy List.minimum model


solveB : Model -> Int
solveB model =
    winnerBy List.maximum model


winnerBy : (List Int -> Maybe Int) -> Model -> Int
winnerBy fn model =
    let
        stepped =
            step 0 model
    in
    case
        stepped.boards
            |> List.filterMap .winnerIdx
            |> fn
            |> Maybe.andThen
                (\idx ->
                    stepped.boards
                        |> List.filter (\b -> b.winnerIdx == Just idx && sumUnMarked b > 0)
                        |> List.head
                )
    of
        Just board ->
            sumUnMarked board
                * Maybe.withDefault 0 board.winNum

        _ ->
            -7777


step : Int -> Model -> Model
step winnerIdx model =
    case model.numbers of
        x :: y ->
            step (winnerIdx + 1)
                { model
                    | boards = List.map (markAndEvaluateIfOpen winnerIdx x) model.boards
                    , numbers = y
                }

        _ ->
            model


markAndEvaluateIfOpen : Int -> Int -> Board -> Board
markAndEvaluateIfOpen winnerIdx x board =
    if board.winnerIdx == Nothing then
        let
            newBoard =
                mark x board
        in
        case evalBoard newBoard of
            Nothing ->
                newBoard

            Just _ ->
                { newBoard | winnerIdx = Just winnerIdx, winNum = Just x }

    else
        board


evalBoard : Board -> Maybe Board
evalBoard board =
    case
        board.cells
            |> Dict.groupBy .y
            |> Dict.filter (\_ v -> allMarked v)
            |> Dict.toList
    of
        [] ->
            case
                board.cells
                    |> Dict.groupBy .x
                    |> Dict.filter (\_ v -> allMarked v)
                    |> Dict.toList
            of
                [] ->
                    Nothing

                _ ->
                    Just board

        _ ->
            Just board


mark : Int -> Board -> Board
mark num board =
    { board | cells = List.map (markCell num) board.cells }


markCell : Int -> Cell -> Cell
markCell num cell =
    { cell | marked = cell.marked || num == cell.val }


sumUnMarked : Board -> Int
sumUnMarked board =
    board.cells
        |> List.filter (.marked >> not)
        |> List.map .val
        |> List.sum


parse : List String -> Model
parse input =
    case input of
        x :: y ->
            { numbers =
                String.split "," x
                    |> List.filterMap String.toInt
            , boards =
                List.groupWhile (\_ b -> b /= "") y
                    |> List.map Tuple.second
                    |> List.map toBoard
            }

        _ ->
            { numbers = []
            , boards = []
            }


toBoard : List String -> Board
toBoard rows =
    { cells =
        rows
            |> List.indexedMap
                (\y row ->
                    String.words row
                        |> List.filterMap String.toInt
                        |> List.indexedMap (\x val -> { x = x, y = y, val = val, marked = False })
                )
            |> List.concat
    , winnerIdx = Nothing
    , winNum = Nothing
    }


allMarked : List Cell -> Bool
allMarked =
    List.all .marked
