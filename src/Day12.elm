module Day12WithGraph exposing (..)

import Dict
import Dict.Extra exposing (groupBy)
import Graph exposing (Edge, Graph, Node, NodeId)
import IntDict
import List.Extra as List exposing (unique)
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


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
                |> IO.map (parse >> toMermaid)
                |> IO.andThen (outputStr "Graph of sample Part A:\n")
            , sample
                |> IO.map (parse >> solveA)
                |> IO.andThen (output "Sample Part A: ")
            , sample2
                |> IO.map (parse >> solveA)
                |> IO.andThen (output "Sample2 Part A: ")
            , sample3
                |> IO.map (parse >> solveA)
                |> IO.andThen (output "Sample3 Part A: ")
            , input
                |> IO.map (parse >> solveA)
                |> IO.andThen (output "Part A: ")
            , sample
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Sample Part B: ")
            , sample2
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Sample2 Part B: ")
            , sample3
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Sample3 Part B: ")
            , input
                |> IO.map (parse >> solveB)
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


solveA : Graph String Int -> Int
solveA graph =
    findAllPaths
        (\path ->
            path
                |> List.filterMap (\nid -> Graph.get nid graph)
                |> groupBy (.node >> .label)
                |> Dict.filter (\k v -> String.toLower k == k && List.length v > 1)
                |> Dict.keys
                |> List.isEmpty
        )
        graph
        |> List.length


solveB : Graph String Int -> Int
solveB graph =
    findAllPaths
        (\path ->
            let
                smallCaveCounts =
                    path
                        |> List.filterMap (\nid -> Graph.get nid graph)
                        |> groupBy (.node >> .label)
                        |> Dict.filter (\k _ -> String.toLower k == k)
                        |> Dict.values
                        |> List.map List.length
            in
            List.all (\ll -> ll <= 2) smallCaveCounts
                && (List.count (\ll -> ll == 2) smallCaveCounts
                        <= 1
                   )
        )
        graph
        |> List.length


findAllPaths : (List NodeId -> Bool) -> Graph String Int -> List (List NodeId)
findAllPaths validFn graph =
    findPaths (findNodeId "end" graph) [ [ findNodeId "start" graph ] ] validFn graph
        |> Tuple.first


findPaths : NodeId -> List (List NodeId) -> (List NodeId -> Bool) -> Graph String Int -> ( List (List NodeId), Graph String Int )
findPaths end remaining validFn graph =
    let
        notEnded =
            remaining
                |> List.filter (\r -> List.last r /= Just end)

        ended =
            remaining
                |> List.filter (\r -> List.last r == Just end)
    in
    if List.isEmpty notEnded then
        ( ended, graph )

    else
        let
            newRem =
                addOutgoingOfTo notEnded graph
                    |> List.filter validFn
        in
        if List.length newRem == List.length notEnded then
            ( ended, graph )

        else
            findPaths end (ended ++ newRem) validFn graph


addOutgoingOfTo : List (List NodeId) -> Graph String Int -> List (List NodeId)
addOutgoingOfTo before graph =
    let
        outgoing nodeid =
            Graph.get nodeid graph
                |> Maybe.map (\ctx -> IntDict.keys ctx.outgoing)
                |> Maybe.withDefault []
    in
    before
        |> List.map
            (\b ->
                let
                    outs =
                        List.last b
                            |> Maybe.map (\l -> outgoing l)
                            |> Maybe.withDefault []
                in
                outs |> List.map (\o -> b ++ [ o ])
            )
        |> List.concat



-- parsing & utils


findNodeId : String -> Graph String Int -> NodeId
findNodeId str =
    Graph.nodes
        >> List.filter (\n -> n.label == str)
        >> List.head
        >> Maybe.map .id
        >> Maybe.withDefault -1


prepare : Graph String Int -> Graph String Int
prepare graph =
    let
        start =
            findNodeId "start" graph

        end =
            findNodeId "end" graph
    in
    Graph.mapContexts
        (\nc ->
            if nc.node.id == start || nc.node.id == end then
                nc

            else
                { nc
                    | incoming = IntDict.union nc.outgoing nc.incoming |> IntDict.remove end
                    , outgoing = IntDict.union nc.incoming nc.outgoing |> IntDict.remove start
                }
        )
        graph


parse : List String -> Graph String Int
parse input =
    let
        nodes =
            allNodes input
    in
    Graph.fromNodesAndEdges nodes (parseEdges nodes input)
        |> prepare


parseEdges : List (Node String) -> List String -> List (Edge Int)
parseEdges nodes lines =
    lines
        |> List.filterMap
            (\line ->
                case String.split "-" line of
                    [ from, to ] ->
                        Maybe.map2
                            (\fromNode toNode ->
                                if to == "start" || from == "end" then
                                    { from = toNode.id, to = fromNode.id, label = 0 }

                                else
                                    { from = fromNode.id, to = toNode.id, label = 0 }
                            )
                            (findNode from nodes)
                            (findNode to nodes)

                    _ ->
                        Nothing
            )


findNode : String -> List (Node String) -> Maybe (Node String)
findNode str nodes =
    nodes |> List.filter (\n -> n.label == str) |> List.head


allNodes : List String -> List (Node String)
allNodes lines =
    lines
        |> List.concatMap
            (\l ->
                l |> String.split "-"
            )
        |> unique
        |> List.indexedMap Node


toMermaid : Graph String Int -> String
toMermaid graph =
    "```mermaid\n"
        ++ "flowchart LR\n"
        ++ (Graph.nodes graph |> List.map (\n -> String.fromInt n.id ++ "[" ++ String.fromInt n.id ++ ":" ++ n.label ++ "]") |> String.join "\n")
        ++ "\n"
        ++ (Graph.edges graph |> List.map (\e -> String.fromInt e.from ++ " --> " ++ String.fromInt e.to) |> String.join "\n")
        ++ "\n"
        ++ "```"
