module Day05 exposing (..)

import Dict
import Dict.Extra exposing (groupBy)
import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Line =
    { from : Pos
    , to : Pos
    }


type alias Model =
    { lines : List Line
    }


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d05" "input"

        sample =
            getInput "d05" "sample"
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
    straightLines model
        |> solveB


solveB : Model -> Int
solveB model =
    model.lines
        |> List.concatMap points
        |> groupBy (\p -> ( p.x, p.y ))
        |> Dict.filter (\k v -> List.length v > 1)
        |> Dict.toList
        |> List.length


points : Line -> List Pos
points line =
    pointStepper line.from line []


pointStepper : Pos -> Line -> List Pos -> List Pos
pointStepper ({ x, y } as cur) line positions =
    let
        lineSlope =
            slope line |> fromTuple
    in
    if cur == line.to then
        cur :: positions

    else
        pointStepper { cur | x = x + lineSlope.x, y = y + lineSlope.y } line (cur :: positions)


straightLines : Model -> Model
straightLines model =
    { model | lines = List.filter isStraight model.lines }


isStraight : Line -> Bool
isStraight { from, to } =
    from.x == to.x || from.y == to.y


slope : Line -> ( Int, Int )
slope { from, to } =
    ( (to.x - from.x) // abs (to.x - from.x), (to.y - from.y) // abs (to.y - from.y) )



---parsing


parse : List String -> Model
parse input =
    { lines = input |> List.filterMap parseLine }


parseLine : String -> Maybe Line
parseLine s =
    case String.split " -> " s |> List.filterMap parsePoint of
        [ p1, p2 ] ->
            Just { from = p1, to = p2 }

        _ ->
            Nothing


toTuple : Pos -> ( Int, Int )
toTuple { x, y } =
    ( x, y )


fromTuple : ( Int, Int ) -> Pos
fromTuple ( x, y ) =
    { x = x, y = y }


parsePoint : String -> Maybe Pos
parsePoint s =
    case String.split "," s |> List.filterMap String.toInt of
        [ x, y ] ->
            Just { x = x, y = y }

        _ ->
            Nothing
