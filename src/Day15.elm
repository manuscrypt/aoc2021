module Day15 exposing (..)

import Dict
import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Problem
import Problem.Example as Problem
import Problem.Search as Problem
import Utils exposing (..)


type alias Pos =
    ( Int, Int )


type alias Model =
    { pos : Pos
    , cells : List Cell
    , path : List Pos
    , risk : Int
    }


type alias Cell =
    { pos : Pos
    , risk : Int
    }


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d15" "input"

        sample =
            getInput "d15" "sample"
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


solveA : Model -> Int
solveA model =
    Problem.routeFinding posToString ( 0, 0 ) (maxPos model.cells) (toGraph model)
        |> solve


solveB : Model -> Int
solveB model =
    let
        bigModel =
            toBigModel model
    in
    Problem.routeFinding posToString ( 0, 0 ) (maxPos bigModel.cells) (toGraph bigModel)
        |> solve


solve : Problem.Problem Pos -> Int
solve problem =
    let
        ( _, solutionModel ) =
            Problem.treeUniformCost problem
                |> Problem.solve

        solution =
            solutionModel
                |> .result
                |> Problem.mapResult .pathCost
    in
    case solution of
        Problem.Pending ->
            -1

        Problem.Solution cost ->
            truncate (cost * 10)

        Problem.Failure ->
            -999


isSurrounding : Pos -> Pos -> Bool
isSurrounding a b =
    List.member b (adjIdxs a)


adjIdxs : ( Int, Int ) -> List ( Int, Int )
adjIdxs ( x, y ) =
    [ ( x + 1, y )
    , ( x - 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]



---parsing


toGraph : Model -> Problem.Graph Pos
toGraph model =
    model.cells
        |> List.map
            (\p ->
                ( p.pos
                , model.cells
                    |> List.filter (\c -> isSurrounding p.pos c.pos)
                    |> List.map (\d -> { stepCost = toFloat d.risk / 10, result = d.pos })
                )
            )
        |> Dict.fromList


toBigModel : Model -> Model
toBigModel model =
    let
        offsets =
            List.range 0 4
                |> List.concatMap
                    (\offY ->
                        List.range 0 4 |> List.map (\offX -> ( offX, offY ))
                    )

        ( maxX, maxY ) =
            maxPos model.cells

        newCells =
            List.foldl
                (\cell all ->
                    List.foldl
                        (\( ox, oy ) a ->
                            let
                                newCost =
                                    cell.risk + ox + oy

                                ( cx, cy ) =
                                    cell.pos
                            in
                            { cell
                                | pos =
                                    ( cx + ox * (maxX + 1)
                                    , cy + oy * (maxY + 1)
                                    )
                                , risk =
                                    if newCost > 9 then
                                        modBy 9 newCost

                                    else
                                        newCost
                            }
                                :: a
                        )
                        all
                        offsets
                )
                []
                model.cells
    in
    { model | cells = newCells }


maxPos : List Cell -> Pos
maxPos cells =
    cells |> List.map .pos |> List.maximum |> Maybe.withDefault ( -1, -1 )


posToString : Pos -> String
posToString pos =
    String.fromInt (Tuple.first pos) ++ "/" ++ String.fromInt (Tuple.second pos)


parse : List String -> Model
parse input =
    { pos = ( 0, 0 )
    , risk = 0
    , cells =
        input
            |> List.indexedMap parseRow
            |> List.concat
    , path = []
    }


parseRow : Int -> String -> List Cell
parseRow row rowString =
    rowString
        |> String.split ""
        |> List.filterMap String.toInt
        |> List.indexedMap (\col i -> Cell ( col, row ) i)
