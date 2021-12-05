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
                |> IO.map (parse >> List.filter isStraight >> solve)
                |> IO.andThen (output "Sample Part A: ")
            , sample
                |> IO.map (parse >> solve)
                |> IO.andThen (output "Sample Part B: ")
            , input
                |> IO.map (parse >> List.filter isStraight >> solve)
                |> IO.andThen (output "Part A: ")
            , input
                |> IO.map (parse >> solve)
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


solve : List Line -> Int
solve lines =
    lines
        |> List.concatMap points
        |> groupBy (\p -> ( p.x, p.y ))
        |> Dict.filter (\_ v -> List.length v > 1)
        |> Dict.toList
        |> List.length


points : Line -> List Pos
points line =
    pointStepper line.from [] line


pointStepper : Pos -> List Pos -> Line -> List Pos
pointStepper cur positions line =
    if cur == line.to then
        cur :: positions

    else
        pointStepper (add cur (slope line)) (cur :: positions) line


add : Pos -> Pos -> Pos
add p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }


isStraight : Line -> Bool
isStraight { from, to } =
    from.x == to.x || from.y == to.y


slope : Line -> Pos
slope { from, to } =
    ( (to.x - from.x) // abs (to.x - from.x)
    , (to.y - from.y) // abs (to.y - from.y)
    )
        |> fromTuple


fromTuple : ( Int, Int ) -> Pos
fromTuple ( x, y ) =
    { x = x, y = y }



---parsing


parse : List String -> List Line
parse input =
    input |> List.filterMap parseLine


parseLine : String -> Maybe Line
parseLine s =
    case String.split " -> " s |> List.filterMap parsePoint of
        [ p1, p2 ] ->
            Just { from = p1, to = p2 }

        _ ->
            Nothing


parsePoint : String -> Maybe Pos
parsePoint s =
    case String.split "," s |> List.filterMap String.toInt of
        [ x, y ] ->
            Just { x = x, y = y }

        _ ->
            Nothing
