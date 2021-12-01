module Day01 exposing (program)

import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


program : Process -> IO ()
program _ =
    let
        input =
            readAllLines "C:/src/aoc2021/data/d01.txt"
                |> toInput
    in
    IO.do
        (IO.combine
            [ sample
                |> partA 0
                |> output "Sample Part A: "
            , sample
                |> partB
                |> output "Sample Part B: "
            , input
                |> IO.map (partA 0)
                |> IO.andThen (output "Part A: ")
            , input
                |> IO.map partB
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


return _ =
    IO.return ()


readAllLines : String -> IO (List String)
readAllLines filename =
    IO.do
        (File.contentsOf filename
            |> IO.exitOnError identity
        )
    <|
        \content ->
            content
                |> String.lines
                |> IO.return


toInput : IO (List String) -> IO (List Int)
toInput lines =
    lines
        |> IO.map (List.filterMap String.toInt)


partA : Int -> List Int -> Int
partA acc nums =
    case nums of
        x :: y :: z ->
            if y > x then
                partA (acc + 1) (y :: z)

            else
                partA acc (y :: z)

        _ ->
            acc


partB : List Int -> Int
partB nums =
    runPartB 0 (List.zip3 nums (List.drop 1 nums) (List.drop 2 nums))


runPartB acc windows =
    case windows of
        ( a0, a1, a2 ) :: ( b0, b1, b2 ) :: c ->
            if (b0 + b1 + b2) > (a0 + a1 + a2) then
                runPartB (acc + 1) (( b0, b1, b2 ) :: c)

            else
                runPartB acc (( b0, b1, b2 ) :: c)

        _ ->
            acc


output : String -> Int -> IO ()
output prefix num =
    IO.do (Proc.print (prefix ++ String.fromInt num)) <| \_ -> IO.return ()


sample : List Int
sample =
    [ 199
    , 200
    , 208
    , 210
    , 200
    , 207
    , 240
    , 269
    , 260
    , 263
    ]
