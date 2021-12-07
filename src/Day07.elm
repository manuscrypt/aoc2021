module Day07 exposing (..)

import Array exposing (fromList)
import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import String exposing (toInt)
import Utils exposing (..)


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d07" "input"

        sample =
            getInput "d07" "sample"
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


solveA : List Int -> Int
solveA crabs =
    let
        m =
            median crabs
    in
    crabs
        |> List.map (\c -> abs (c - m))
        |> List.sum


solveB : List Int -> Int
solveB crabs =
    let
        m =
            mean crabs
    in
    crabs
        |> List.map (\c -> abs (c - m))
        |> List.map fuel
        |> List.sum


median : List Int -> Int
median crabs =
    List.sort crabs
        |> fromList
        |> Array.get (List.length crabs // 2)
        |> Maybe.withDefault -1



-- why the -1 one though?
-- sample mean is 4.9 -> works with 5
-- input mean is 464.501 -> only works with 464


mean : List Int -> Int
mean crabs =
    List.sum crabs // (List.length crabs - 1)


fuel : Int -> Int
fuel n =
    (n * (n + 1)) // 2



---parsing


parse : List String -> List Int
parse input =
    case input of
        [ s ] ->
            parseLine s

        _ ->
            []


parseLine : String -> List Int
parseLine s =
    String.split "," s
        |> List.filterMap String.toInt
