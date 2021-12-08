module Day08 exposing (..)

import Array exposing (fromList)
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
        first =
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
                                    case ( p.digit, countCommon pat4.str p.str ) of
                                        ( Nothing, 4 ) ->
                                            { p | digit = Just 9 }

                                        _ ->
                                            p
                                )
                    )
                |> Maybe.withDefault filled

        withThree =
            getPatternForDigit 7 withNine
                |> Maybe.map
                    (\pat7 ->
                        withNine
                            |> List.map
                                (\p ->
                                    case ( p.digit, countCommon pat7.str p.str, countDiff p.str pat7.str ) of
                                        ( Nothing, 3, 2 ) ->
                                            { p | digit = Just 3 }

                                        ( Nothing, 3, 3 ) ->
                                            { p | digit = Just 0 }

                                        _ ->
                                            p
                                )
                    )
                |> Maybe.withDefault withNine

        withSix =
            withThree
                |> List.map
                    (\p ->
                        case ( p.digit, String.length p.str ) of
                            ( Nothing, 6 ) ->
                                { p | digit = Just 6 }

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
                                    case ( p.digit, countCommon pat4.str p.str ) of
                                        ( Nothing, 3 ) ->
                                            { p | digit = Just 5 }

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
                            { p | digit = Just 2 }

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


countCommon : String -> String -> Int
countCommon s1 s2 =
    Set.intersect (Set.fromList (String.split "" s1)) (Set.fromList (String.split "" s2))
        |> Set.size


countDiff : String -> String -> Int
countDiff s1 s2 =
    Set.diff (Set.fromList (String.split "" s1)) (Set.fromList (String.split "" s2))
        |> Set.size


fillKnown : Pattern -> Pattern
fillKnown p =
    case len p of
        2 ->
            { p | digit = Just 1 }

        3 ->
            { p | digit = Just 7 }

        4 ->
            { p | digit = Just 4 }

        7 ->
            { p | digit = Just 8 }

        _ ->
            p


len : Pattern -> Int
len pattern =
    String.length pattern.str


getPatternWithLength : Int -> List Pattern -> List Pattern
getPatternWithLength i patterns =
    patterns
        |> List.filter (\s -> String.length s.str == i)


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
