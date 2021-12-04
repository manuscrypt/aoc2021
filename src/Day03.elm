module Day03 exposing (program)

import Array exposing (Array)
import Binary
import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d03" "input"

        sample =
            getInput "d03" "sample"
    in
    IO.do
        (IO.combine
            [ sample
                |> IO.map solveA
                |> IO.andThen (output "Sample Part A: ")
            , sample
                |> IO.map solveB
                |> IO.andThen (output "Sample Part B: ")
            , input
                |> IO.map solveA
                |> IO.andThen (output "Part A: ")
            , input
                |> IO.map solveB
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


solveA : List String -> Int
solveA strings =
    let
        counts =
            recombine strings
                |> Dict.map (\_ v -> ( count "0" v, count "1" v ))
                |> Dict.values

        gamma =
            counts |> List.map (getSignificantBitInt True) |> Binary.fromIntegers |> Binary.toDecimal

        epsilon =
            counts |> List.map (getSignificantBitInt False) |> Binary.fromIntegers |> Binary.toDecimal
    in
    gamma * epsilon


solveB : List String -> Int
solveB strings =
    let
        remaining =
            toStringArrays strings

        oxygen =
            stepB True 0 remaining

        co2 =
            stepB False 0 remaining
    in
    oxygen * co2


stepB : Bool -> Int -> List (Array String) -> Int
stepB most pos remaining =
    case remaining of
        [ x ] ->
            x
                |> Array.toList
                |> List.filterMap String.toInt
                |> Binary.fromIntegers
                |> Binary.toDecimal

        _ ->
            let
                recombined =
                    recombine (List.map (Array.toList >> String.join "") remaining)
            in
            case Dict.get pos recombined of
                Nothing ->
                    -99

                Just v ->
                    let
                        filterBit =
                            getSignificantBit most v

                        filtered =
                            remaining
                                |> List.filter (\i -> Array.get pos i == Just filterBit)
                    in
                    stepB most (pos + 1) filtered


count : String -> List String -> Int
count c =
    List.filter ((==) c) >> List.length


getSignificantBitInt : Bool -> ( Int, Int ) -> Int
getSignificantBitInt most ( zeroes, ones ) =
    if most then
        if zeroes > ones then
            0

        else
            1

    else if zeroes > ones then
        1

    else
        0


getSignificantBit : Bool -> List String -> String
getSignificantBit most list =
    let
        ( zeroes, ones ) =
            ( count "0" list, count "1" list )
    in
    if most then
        if ones >= zeroes then
            "1"

        else
            "0"

    else if zeroes <= ones then
        "0"

    else
        "1"


recombine : List String -> Dict Int (List String)
recombine strings =
    strings
        |> List.map (String.split "")
        |> List.concatMap (List.indexedMap Tuple.pair)
        |> Dict.groupBy Tuple.first
        |> Dict.map (\_ v -> List.map Tuple.second v)


toStringArrays : List String -> List (Array String)
toStringArrays strings =
    strings
        |> List.map (String.split "" >> Array.fromList)
