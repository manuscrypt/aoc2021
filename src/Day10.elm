module Day10 exposing (..)

import Array
import Posix.IO as IO exposing (IO, Process)
import Stack exposing (Stack)
import Utils exposing (..)


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d10" "input"

        sample =
            getInput "d10" "sample"
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


solveA : List (List Char) -> Int
solveA input =
    input
        |> List.map inspectLine
        |> List.map Tuple.second
        |> List.sum


solveB : List (List Char) -> Int
solveB input =
    input
        |> List.map inspectLine
        |> List.filter (\x -> Tuple.second x == 0)
        |> List.map Tuple.first
        |> List.map
            (\stack ->
                Stack.toList stack
                    |> List.map (Char.toCode >> openToClosing >> Char.fromCode)
                    |> List.foldl (\c score -> score * 5 + scoreB c) 0
            )
        |> List.sort
        |> Array.fromList
        |> (\arr ->
                Array.get (Array.length arr // 2) arr
                    |> Maybe.withDefault -1
           )


inspectLine : List Char -> ( Stack Char, Int )
inspectLine chars =
    step chars ( Stack.initialise, 0 )


step : List Char -> ( Stack Char, Int ) -> ( Stack Char, Int )
step chars ( stack, prevScore ) =
    case chars of
        [] ->
            ( stack, prevScore )

        c :: rest ->
            if List.member c openers then
                step rest ( Stack.push c stack, prevScore )

            else if Stack.top stack == Just (Char.toCode c |> closingToOpen |> Char.fromCode) then
                step rest ( Stack.pop stack |> Tuple.second, prevScore )

            else
                ( stack, scoreA c )


openers : List Char
openers =
    [ '('
    , '['
    , '{'
    , '<'
    ]


delta : Int -> Int
delta code =
    if code < 42 then
        1

    else
        2


openToClosing : Int -> Int
openToClosing code =
    code + delta code


closingToOpen : Int -> Int
closingToOpen code =
    code - delta code


scoreA : Char -> Int
scoreA c =
    case c of
        ')' ->
            3

        ']' ->
            57

        '}' ->
            1197

        '>' ->
            25137

        _ ->
            -9999


scoreB : Char -> Int
scoreB c =
    case c of
        ')' ->
            1

        ']' ->
            2

        '}' ->
            3

        '>' ->
            4

        _ ->
            -9999



---parsing


parse : List String -> List (List Char)
parse input =
    input
        |> List.map String.toList
