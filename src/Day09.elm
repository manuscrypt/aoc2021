module Day09 exposing (..)

import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type alias Cell =
    { pos : ( Int, Int )
    , v : Int
    }


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d09" "input"

        sample =
            getInput "d09" "sample"
    in
    IO.do
        (IO.combine
            [ sample
                |> IO.map (parse >> solveA)
                |> IO.andThen (output "Sample Part A: ")
            , sample
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Sample Part B: ")

            -- , input
            --     |> IO.map (parse >> solveA)
            --     |> IO.andThen (output "Part A: ")
            , input
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


solveA : List Cell -> Int
solveA cells =
    getLowPoints cells
        |> List.map (\c -> 1 + c.v)
        |> List.sum


solveB : List Cell -> Int
solveB cells =
    getLowPoints cells
        |> List.map
            (\lp ->
                findBasins (List.filter (\c -> c.v /= 9) cells) [] [ lp ]
            )
        |> List.map (\l -> List.length l + 1)
        |> List.sort
        |> List.reverse
        |> List.take 3
        |> List.foldl (*) 1


findBasins : List Cell -> List Cell -> List Cell -> List Cell
findBasins cells foundSoFar remainingToCheck =
    case remainingToCheck of
        [] ->
            foundSoFar

        x :: y ->
            let
                checkOnlyThese =
                    List.filter (\c -> not (List.any (\f -> f.pos == c.pos) foundSoFar)) cells

                nextHighers =
                    getSurroundingCellsWithHigher x checkOnlyThese

                nextFound =
                    (foundSoFar ++ nextHighers) |> List.uniqueBy .pos
            in
            findBasins checkOnlyThese nextFound (y ++ nextHighers)


getSurroundingCellsWithHigher : Cell -> List Cell -> List Cell
getSurroundingCellsWithHigher lowPoint cells =
    getSurroundingCells lowPoint.pos cells
        |> List.filter (\c -> lowPoint.v < c.v)


getLowPoints : List Cell -> List Cell
getLowPoints cells =
    cells
        |> List.filter
            (\c ->
                getSurroundingCells c.pos cells |> List.all (\x -> x.v > c.v)
            )


getSurroundingCells : ( Int, Int ) -> List Cell -> List Cell
getSurroundingCells ( x, y ) list =
    let
        surrounding =
            [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]
    in
    list |> List.filter (\c -> List.member c.pos surrounding)



---parsing


parse : List String -> List Cell
parse input =
    input
        |> List.indexedMap parseLine
        |> List.concat


parseLine : Int -> String -> List Cell
parseLine row line =
    String.split "" line
        |> List.indexedMap (\col s -> String.toInt s |> Maybe.map (\v -> Cell ( row, col ) v))
        |> List.filterMap identity
