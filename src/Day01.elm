module Day01 exposing (program)

import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type Window
    = Single Int
    | Triplet ( Int, Int, Int )


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d01" "input"

        sample =
            getInput "d01" "sample"
    in
    IO.do
        (IO.combine
            [ sample
                |> IO.map (toInputA >> count 0)
                |> IO.andThen (output "Sample Part A: ")
            , sample
                |> IO.map (toInputB >> count 0)
                |> IO.andThen (output "Sample Part B: ")
            , input
                |> IO.map (toInputA >> count 0)
                |> IO.andThen (output "Part A: ")
            , input
                |> IO.map (toInputB >> count 0)
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


toInputA : List String -> List Window
toInputA =
    List.filterMap String.toInt >> List.map Single


toInputB : List String -> List Window
toInputB =
    List.filterMap String.toInt
        >> (\nums -> List.zip3 nums (List.drop 1 nums) (List.drop 2 nums))
        >> List.map Triplet


count : Int -> List Window -> Int
count acc windows =
    case windows of
        x :: y :: z ->
            count (addOneIfSumGreater y x acc) (y :: z)

        _ ->
            acc


addOneIfSumGreater : Window -> Window -> Int -> Int
addOneIfSumGreater a b acc =
    if sum a > sum b then
        acc + 1

    else
        acc


sum : Window -> Int
sum win =
    case win of
        Single i ->
            i

        Triplet ( a, b, c ) ->
            a + b + c
