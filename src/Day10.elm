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
    let
        scores =
            input
                |> List.map inspectLine
                |> List.filter (\x -> Tuple.second x == 0)
                |> List.map Tuple.first
                |> List.map
                    (\stack ->
                        Stack.toList stack
                            |> List.map closerOf
                            |> List.foldl (\c score -> score * 5 + scoreB c) 0
                    )
                |> List.sort
                |> Array.fromList
    in
    Array.get (Array.length scores // 2) scores |> Maybe.withDefault -1


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

            else if Stack.top stack == Just (openerOf c) then
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


openerOf : Char -> Char
openerOf c =
    case c of
        ')' ->
            '('

        ']' ->
            '['

        '}' ->
            '{'

        '>' ->
            '<'

        _ ->
            Debug.todo "oops3"


closerOf : Char -> Char
closerOf c =
    case c of
        '(' ->
            ')'

        '[' ->
            ']'

        '{' ->
            '}'

        '<' ->
            '>'

        _ ->
            Debug.todo "oops9"


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
            Debug.todo "oops2"


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
            Debug.todo "oops5"



---parsing


parse : List String -> List (List Char)
parse input =
    input
        |> List.map String.toList
