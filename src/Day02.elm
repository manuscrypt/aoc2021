module Day02 exposing (program)

import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type alias Track =
    ( Int, Int, Int )


type Move
    = Forward Int
    | Down Int
    | Up Int


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d02" "input"

        sample =
            getInput "d02" "sample"
    in
    IO.do
        (IO.combine
            [ sample
                |> IO.map (toInput >> count evalA)
                |> IO.andThen (output "Sample Part A: ")
            , sample
                |> IO.map (toInput >> count evalB)
                |> IO.andThen (output "Sample Part B: ")
            , input
                |> IO.map (toInput >> count evalA)
                |> IO.andThen (output "Part A: ")
            , input
                |> IO.map (toInput >> count evalB)
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


count : (Move -> Track -> Track) -> List Move -> Int
count evalFn moves =
    let
        ( rHoriz, rDepth, _ ) =
            List.foldl evalFn ( 0, 0, 0 ) moves
    in
    rHoriz * rDepth


evalA : Move -> Track -> Track
evalA move ( horiz, depth, aim ) =
    case move of
        Forward i ->
            ( horiz + i, depth, aim )

        Down i ->
            ( horiz, depth + i, aim )

        Up i ->
            ( horiz, depth - i, aim )


evalB : Move -> Track -> Track
evalB move ( horiz, depth, aim ) =
    case move of
        Forward i ->
            ( horiz + i, depth + (aim * i), aim )

        Down i ->
            ( horiz, depth, aim + i )

        Up i ->
            ( horiz, depth, aim - i )


toInput : List String -> List Move
toInput =
    List.filterMap parseMove


parseMove : String -> Maybe Move
parseMove str =
    case String.split " " str of
        [ move, i ] ->
            String.toInt i
                |> Maybe.andThen
                    (\amt ->
                        case move of
                            "forward" ->
                                Forward amt |> Just

                            "up" ->
                                Up amt |> Just

                            "down" ->
                                Down amt |> Just

                            _ ->
                                Nothing
                    )

        _ ->
            Nothing
