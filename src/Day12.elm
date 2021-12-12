module Day12 exposing (..)

import Dict
import Dict.Extra as Dict
import Graph exposing (Edge, Graph)
import List.Extra as List exposing (unique, uniqueBy)
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type alias Edge a e =
    { from : a, to : a, label : e }


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d12" "input"

        sample =
            getInput "d12" "sample"

        sample2 =
            getInput "d12" "sample2"

        sample3 =
            getInput "d12" "sample3"
    in
    IO.do
        (IO.combine
            [ sample
                |> IO.map (parse >> solveA)
                |> IO.andThen (output "Sample Part A: ")
            , sample2
                |> IO.map (parse >> solveA)
                |> IO.andThen (output "Sample2 Part A: ")
            , sample3
                |> IO.map (parse >> solveA)
                |> IO.andThen (output "Sample3 Part A: ")

            -- , input
            --     |> IO.map (parse >> solveA)
            --     |> IO.andThen (output "Part A: ")
            , sample
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Sample Part B: ")
            , sample2
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Sample2 Part B: ")

            -- , sample3
            --     |> IO.map (parse >> solveB)
            --     |> IO.andThen (output "Sample3 Part B: ")
            , input
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


solveA : List (Edge String Int) -> Int
solveA edges =
    findPaths edges [ [ "start" ] ] expandPath validPathA
        |> List.filter endsInEnd
        |> List.length


solveB : List (Edge String Int) -> Int
solveB edges =
    findPaths edges [ [ "start" ] ] expandPathB validPathB
        |> List.filter endsInEnd
        |> List.length


findPaths : List (Edge String Int) -> List (List String) -> (List String -> List (Edge String Int) -> List (List String)) -> (List String -> Bool) -> List (List String)
findPaths allEdges allPaths expandFn validFn =
    let
        notEnded =
            allPaths
                |> List.filter (\p -> List.last p /= Just "end")

        ended =
            allPaths
                |> List.filter (\p -> List.last p == Just "end")
    in
    if List.isEmpty notEnded then
        allPaths
        -- |> List.filter validFn

    else
        let
            expanded =
                notEnded
                    |> List.concatMap (\ne -> expandFn ne allEdges)
                    |> List.filter validFn
        in
        if Debug.log "all+new" (List.length (allPaths ++ expanded |> unique)) == Debug.log "all" (List.length allPaths) then
            allPaths
            --|> List.filter validFn

        else
            findPaths allEdges (ended ++ expanded |> unique) expandFn validFn


endsInEnd : List String -> Bool
endsInEnd path =
    List.last path == Just "end"


validPathA : List String -> Bool
validPathA path =
    path
        |> Dict.groupBy identity
        |> Dict.map
            (\k v ->
                if String.toLower k == k then
                    List.length v == 1

                else
                    True
            )
        |> Dict.values
        |> List.all identity


validPathB : List String -> Bool
validPathB path =
    let
        smallCaveCounts =
            path
                |> Dict.groupBy identity
                |> Dict.map
                    (\k v ->
                        if String.toLower k == k then
                            Just ( k, List.length v )

                        else
                            Nothing
                    )
                |> Dict.values
                |> List.filterMap identity
    in
    List.all (\x -> Tuple.second x < 3) smallCaveCounts
        && (List.count (\x -> Tuple.second x == 2) smallCaveCounts <= 1)


expandPath : List String -> List (Edge String Int) -> List (List String)
expandPath path edges =
    case List.last path of
        Nothing ->
            []

        Just p ->
            getOutgoing p path edges
                |> List.map (\og -> path ++ [ og ])


expandPathB : List String -> List (Edge String Int) -> List (List String)
expandPathB path edges =
    case List.last path of
        Nothing ->
            []

        Just p ->
            if p == "end" then
                [ path ]

            else
                getOutgoingB p path edges
                    |> List.map (\og -> path ++ [ og ])


getOutgoing : String -> List String -> List (Edge String Int) -> List String
getOutgoing str pathSoFar edges =
    edges
        |> List.filterMap
            (\e ->
                if e.from == str && e.from /= "end" then
                    if e.to /= "start" then
                        if not (contains pathSoFar ( e.from, e.to )) then
                            Just e.to

                        else
                            Nothing

                    else
                        Nothing

                else if e.to == str && e.to /= "end" then
                    if e.from /= "start" then
                        if not (contains pathSoFar ( e.to, e.from )) then
                            Just e.from

                        else
                            Nothing

                    else
                        Nothing

                else
                    Nothing
            )


getOutgoingB : String -> List String -> List (Edge String Int) -> List String
getOutgoingB str pathSoFar edges =
    edges
        |> List.filterMap
            (\e ->
                if e.from == str && e.from /= "end" then
                    if e.to /= "start" then
                        if not (containsTwice pathSoFar ( e.from, e.to )) then
                            Just e.to

                        else
                            Nothing

                    else
                        Nothing

                else if e.to == str && e.to /= "end" then
                    if e.from /= "start" then
                        if not (containsTwice pathSoFar ( e.to, e.from )) then
                            Just e.from

                        else
                            Nothing

                    else
                        Nothing

                else
                    Nothing
            )


contains : List String -> ( String, String ) -> Bool
contains strs ( from, to ) =
    strs |> String.join "" |> String.contains (from ++ to)


containsTwice : List String -> ( String, String ) -> Bool
containsTwice strs ( from, to ) =
    (strs |> String.join "" |> String.split (from ++ to) |> List.length) >= 2



---parsing


parse : List String -> List (Edge String Int)
parse input =
    List.filterMap parseLine input


parseLine : String -> Maybe (Edge String Int)
parseLine line =
    case line |> String.split "-" of
        [ from, to ] ->
            Just { from = from, to = to, label = 0 }

        _ ->
            Nothing
