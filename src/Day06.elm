module Day06 exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type alias Fishes =
    Dict Int Int


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d06" "input"

        sample =
            getInput "d06" "sample"
    in
    IO.do
        (IO.combine
            [ sample
                |> IO.map (parse >> solve 80)
                |> IO.andThen (output "Sample Part A: ")
            , sample
                |> IO.map (parse >> solve 256)
                |> IO.andThen (output "Sample Part B: ")
            , input
                |> IO.map (parse >> solve 80)
                |> IO.andThen (output "Part A: ")
            , input
                |> IO.map (parse >> solve 256)
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


solve : Int -> Fishes -> Int
solve days fishes =
    step days fishes
        |> Dict.values
        |> List.sum


step : Int -> Fishes -> Fishes
step daysLeft fishes =
    if daysLeft == 0 then
        fishes

    else
        let
            zeroes =
                fishes
                    |> Dict.get 0
                    |> Maybe.withDefault 0

            newFishes =
                fishes
                    |> Dict.mapKeys (\day -> day - 1)
                    |> Dict.remove -1

            recycled =
                if zeroes > 0 then
                    newFishes
                        |> Dict.update 6 (\mbv -> Just (Maybe.withDefault 0 mbv + zeroes))
                        |> Dict.insert 8 zeroes

                else
                    newFishes
        in
        step (daysLeft - 1) recycled



---parsing


parse : List String -> Fishes
parse input =
    case input of
        [ s ] ->
            parseLine s

        _ ->
            Dict.empty


parseLine : String -> Fishes
parseLine s =
    String.split "," s
        |> List.filterMap String.toInt
        |> Dict.groupBy identity
        |> Dict.map (\_ v -> List.length v)
