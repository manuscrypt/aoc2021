module Day11 exposing (..)

import List.Extra as List exposing (uniqueBy)
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type alias Cell =
    { pos : ( Int, Int )
    , cur : Int
    , flashes : Int
    , hasFlashedThisRound : Bool
    }


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d11" "input"

        sample =
            getInput "d11" "sample"
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


solveA : List Cell -> Int
solveA cells =
    iter cells 0 100
        |> Tuple.second
        |> List.map .flashes
        |> List.sum


solveB : List Cell -> Int
solveB cells =
    iter cells 0 2000 |> Tuple.first |> (+) 1


iter : List Cell -> Int -> Int -> ( Int, List Cell )
iter cells currentStep stepTarget =
    if currentStep < stepTarget then
        let
            increased =
                incBy1 cells

            flashingCells =
                flashers increased

            ( allFlashed, newCells ) =
                flashOut increased flashingCells
        in
        if allFlashed then
            ( currentStep, newCells )

        else
            iter newCells (currentStep + 1) stepTarget

    else
        ( currentStep, cells )


flashOut : List Cell -> List Cell -> ( Bool, List Cell )
flashOut cells remaining =
    case remaining of
        x :: y ->
            let
                incSurrounding =
                    cells
                        |> List.map
                            (\c ->
                                if isSurrounding x c then
                                    { c | cur = c.cur + 1 }

                                else if c.pos == x.pos then
                                    { c | hasFlashedThisRound = True, flashes = c.flashes + 1 }

                                else
                                    c
                            )
            in
            flashOut incSurrounding (flashers incSurrounding ++ y |> uniqueBy .pos)

        [] ->
            ( List.length cells == List.count .hasFlashedThisRound cells
            , resetCells cells
            )


resetCells : List Cell -> List Cell
resetCells =
    List.map
        (\c ->
            if c.hasFlashedThisRound then
                { c | cur = 0, hasFlashedThisRound = False }

            else
                c
        )


incBy1 : List Cell -> List Cell
incBy1 cells =
    cells |> List.map (\c -> { c | cur = c.cur + 1 })


flashers : List Cell -> List Cell
flashers cells =
    cells |> List.filter (\c -> c.cur > 9 && not c.hasFlashedThisRound)


isSurrounding : Cell -> Cell -> Bool
isSurrounding a b =
    List.member b.pos (adjIdxs a.pos)


adjIdxs : ( Int, Int ) -> List ( Int, Int )
adjIdxs ( x, y ) =
    [ ( x - 1, y - 1 ), ( x, y - 1 ), ( x + 1, y - 1 ), ( x - 1, y ), ( x + 1, y ), ( x - 1, y + 1 ), ( x, y + 1 ), ( x + 1, y + 1 ) ]



---parsing


parse : List String -> List Cell
parse input =
    input
        |> List.indexedMap parseRow
        |> List.concat


parseRow : Int -> String -> List Cell
parseRow row rowString =
    rowString
        |> String.split ""
        |> List.filterMap String.toInt
        |> List.indexedMap (\col i -> Cell ( col, row ) i 0 False)
