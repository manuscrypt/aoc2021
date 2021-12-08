module Day08 exposing (..)

import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Set
import Utils exposing (..)


type alias Pattern =
    { str : String
    , digit : Maybe Int
    }


type alias Entry =
    { patterns : List Pattern
    , outputDigits : List Pattern
    }


program : Process -> IO ()
program _ =
    let
        _ =
            [ "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf" ]
                |> parse
                |> solveB
                |> Debug.log "x"

        input =
            getInput "d08" "input"

        sample =
            getInput "d08" "sample"
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


solveA : List Entry -> Int
solveA entries =
    entries
        |> List.concatMap .outputDigits
        |> List.map fillKnown
        |> List.filter (\s -> s.digit /= Nothing)
        |> List.length


solveB : List Entry -> Int
solveB entries =
    entries |> List.map solveEntry |> List.sum


solveEntry : Entry -> Int
solveEntry entry =
    let
        filled =
            List.map fillKnown entry.patterns

        withNine =
            getPatternForDigit 4 filled
                |> Maybe.map
                    (\pat4 ->
                        filled
                            |> List.map
                                (\p ->
                                    case ( p.digit, countCommon pat4 p ) of
                                        ( Nothing, 4 ) ->
                                            setDigit 9 p

                                        _ ->
                                            p
                                )
                    )
                |> Maybe.withDefault filled

        withThreeAndZero =
            getPatternForDigit 7 withNine
                |> Maybe.map
                    (\pat7 ->
                        withNine
                            |> List.map
                                (\p ->
                                    case ( p.digit, countCommon pat7 p, countDiff p pat7 ) of
                                        ( Nothing, 3, 2 ) ->
                                            setDigit 3 p

                                        ( Nothing, 3, 3 ) ->
                                            setDigit 0 p

                                        _ ->
                                            p
                                )
                    )
                |> Maybe.withDefault withNine

        withSix =
            withThreeAndZero
                |> List.map
                    (\p ->
                        case ( p.digit, len p ) of
                            ( Nothing, 6 ) ->
                                setDigit 6 p

                            _ ->
                                p
                    )

        withFive =
            getPatternForDigit 4 filled
                |> Maybe.map
                    (\pat4 ->
                        withSix
                            |> List.map
                                (\p ->
                                    case ( p.digit, countCommon pat4 p ) of
                                        ( Nothing, 3 ) ->
                                            setDigit 5 p

                                        _ ->
                                            p
                                )
                    )
                |> Maybe.withDefault withSix

        withTwo =
            withFive
                |> List.map
                    (\p ->
                        if p.digit == Nothing then
                            setDigit 2 p

                        else
                            p
                    )
    in
    translate withTwo entry.outputDigits


translate : List Pattern -> List Pattern -> Int
translate filled empty =
    empty
        |> List.map (\e -> getDigitForPattern e.str filled |> Maybe.withDefault 0)
        |> List.map2 (*) [ 1000, 100, 10, 1 ]
        |> List.sum


countCommon : Pattern -> Pattern -> Int
countCommon p1 p2 =
    Set.intersect (Set.fromList (String.split "" p1.str)) (Set.fromList (String.split "" p2.str))
        |> Set.size


countDiff : Pattern -> Pattern -> Int
countDiff p1 p2 =
    Set.diff (Set.fromList (String.split "" p1.str)) (Set.fromList (String.split "" p2.str))
        |> Set.size


fillKnown : Pattern -> Pattern
fillKnown p =
    case len p of
        2 ->
            setDigit 1 p

        3 ->
            setDigit 7 p

        4 ->
            setDigit 4 p

        7 ->
            setDigit 8 p

        _ ->
            p


setDigit : Int -> Pattern -> Pattern
setDigit i p =
    { p | digit = Just i }


len : Pattern -> Int
len pattern =
    String.length pattern.str


getPatternForDigit : Int -> List Pattern -> Maybe Pattern
getPatternForDigit i patterns =
    patterns |> List.filter (\p -> p.digit == Just i) |> List.head


getDigitForPattern : String -> List Pattern -> Maybe Int
getDigitForPattern str filled =
    filled
        |> List.filter (\p -> p.str == str)
        |> List.head
        |> Maybe.andThen .digit



---parsing


parse : List String -> List Entry
parse input =
    input
        |> List.filterMap parseLine


parseLine : String -> Maybe Entry
parseLine line =
    case String.split " | " line of
        [ left, right ] ->
            Entry
                (String.words left |> List.map toPattern)
                (String.words right |> List.map toPattern)
                |> Just

        _ ->
            Nothing


toPattern : String -> Pattern
toPattern str =
    Pattern
        (String.split "" str
            |> List.sort
            |> String.join ""
        )
        Nothing
