module Day13 exposing (..)

import List.Extra as List exposing (unique)
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type alias Pos =
    ( Int, Int )


type Fold
    = X Int
    | Y Int


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d13" "input"

        sample =
            getInput "d13" "sample"
    in
    IO.do
        (IO.combine
            [ sample
                |> IO.map (parse >> solveA [ Y 7 ])
                |> IO.andThen (output "Sample Part A: ")
            , input
                |> IO.map (parse >> solveA foldsInput)
                |> IO.andThen (output "Part A: ")
            , sample
                |> IO.map (parse >> solveB foldsSample)
                |> IO.andThen (output "Sample Part B: ")
            , input
                |> IO.map (parse >> solveB foldsInput)
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


solveA : List Fold -> List Pos -> Int
solveA folds cells =
    foldAll folds cells
        |> List.length


solveB : List Fold -> List Pos -> Int
solveB folds cells =
    foldAll folds cells
        |> List.sort
        |> Debug.log "folded"
        |> List.length


foldAll : List Fold -> List Pos -> List Pos
foldAll folds cells =
    List.foldl
        (\f c ->
            case f of
                Y y ->
                    foldY y c

                X x ->
                    foldX x c
        )
        cells
        folds


foldY : Int -> List Pos -> List Pos
foldY y positions =
    positions
        |> List.map
            (\( px, py ) ->
                let
                    diff =
                        py - y
                in
                if diff > 0 then
                    ( px, y - diff )

                else
                    ( px, py )
            )
        |> unique


foldX : Int -> List Pos -> List Pos
foldX x positions =
    positions
        |> List.map
            (\( px, py ) ->
                let
                    diff =
                        px - x
                in
                if diff > 0 then
                    ( x - diff, py )

                else
                    ( px, py )
            )
        |> unique



---parsing


parse : List String -> List Pos
parse input =
    input
        |> List.filterMap
            (\line ->
                case String.split "," line of
                    [ x, y ] ->
                        Just ( String.toInt x |> Maybe.withDefault -1, String.toInt y |> Maybe.withDefault -1 )

                    _ ->
                        Nothing
            )


foldsSample : List Fold
foldsSample =
    [ Y 7, X 5 ]


foldsInput : List Fold
foldsInput =
    [ X 655
    , Y 447
    , X 327
    , Y 223
    , X 163
    , Y 111
    , X 81
    , Y 55
    , X 40
    , Y 27
    , Y 13
    , Y 6
    ]
