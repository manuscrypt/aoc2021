module Day01 exposing (program)

import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


type Window
    = Single Int
    | Triplet ( Int, Int, Int )


program : Process -> IO ()
program _ =
    let
        input =
            readAllLines "C:/src/aoc2021/data/d01.txt"
    in
    IO.do
        (IO.combine
            [ sample
                |> toInputA
                |> count 0
                |> output "Sample Part A: "
            , sample
                |> toInputB
                |> count 0
                |> output "Sample Part B: "
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


sample : List String
sample =
    [ "199"
    , "200"
    , "208"
    , "210"
    , "200"
    , "207"
    , "240"
    , "269"
    , "260"
    , "263"
    ]


output : String -> Int -> IO ()
output prefix num =
    IO.do (Proc.print (prefix ++ String.fromInt num))
        return


readAllLines : String -> IO (List String)
readAllLines filename =
    IO.do
        (File.contentsOf filename
            |> IO.exitOnError identity
        )
        (String.lines
            >> IO.return
        )


return : a -> IO ()
return _ =
    IO.return ()
