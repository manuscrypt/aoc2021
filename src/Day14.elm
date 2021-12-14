module Day14 exposing (..)

import Dict exposing (Dict)
import Dict.Extra exposing (groupBy)
import List.Extra as List exposing (unique)
import Posix.IO as IO exposing (IO, Process)
import String exposing (right)
import Utils exposing (..)


type alias Rule =
    { pattern : String
    , replacement : String
    }


type alias Node =
    { left : String
    , right : String
    , count : Int
    , level : Int
    }


type alias Model =
    { template : String
    , rules : List Rule
    , nodes : Dict String Node
    }


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d14" "input"

        sample =
            getInput "d14" "sample"
    in
    IO.do
        (IO.combine
            [ sample
                |> IO.map (parse >> solve 0 10)
                |> IO.andThen (output "Sample Part A: ")
            , input
                |> IO.map (parse >> solve 0 10)
                |> IO.andThen (output "Part A: ")
            , sample
                |> IO.map (parse >> solve 0 40)
                |> IO.andThen (output "Sample Part B: ")
            , input
                |> IO.map (parse >> solve 0 40)
                |> IO.andThen (output "Part B: ")
            ]
        )
        return


solve : Int -> Int -> Model -> Int
solve steps maxSteps model =
    if steps < maxSteps then
        solve (steps + 1) maxSteps (activateNodes (steps + 1) model)

    else
        let
            ( min, max ) =
                nodeCounts (steps + 1) (replacements model) model.nodes
        in
        max
            - min
            + (if maxSteps == 10 then
                1

               else
                -1
              )


replacements : Model -> List String
replacements model =
    model.rules |> List.map .replacement |> unique


activateNodes : Int -> Model -> Model
activateNodes level model =
    let
        targets =
            Dict.filter (\_ v -> v.level == level) model.nodes
                |> Dict.toList
                |> List.map Tuple.second
                |> List.concatMap (\c -> [ ( c.left, c.count ), ( c.right, c.count ) ])
                |> groupBy Tuple.first
                |> Dict.map (\_ v -> List.map Tuple.second v)
                |> Dict.toList
                |> List.map (\( pat, cnt ) -> ( pat, List.sum cnt ))
    in
    if List.isEmpty targets then
        model

    else
        { model
            | nodes =
                List.foldl
                    (\( pat, count ) d ->
                        d |> Dict.update pat (\mbc -> Maybe.map (activateNode (level + 1) count) mbc)
                    )
                    model.nodes
                    targets
        }


activateNode : Int -> Int -> Node -> Node
activateNode level count cell =
    { cell
        | count = count
        , level = level
    }


nodeCounts : Int -> List String -> Dict String Node -> ( Int, Int )
nodeCounts step characters nodes =
    let
        counts =
            characters
                |> List.map
                    (\char ->
                        Dict.filter (\k v -> v.level == step && char == String.left 1 k) nodes
                            |> Dict.values
                            |> List.map .count
                            |> List.sum
                    )
    in
    ( counts |> List.minimum |> Maybe.withDefault 0
    , counts |> List.maximum |> Maybe.withDefault 0
    )



---parsing


createNodes : Model -> Model
createNodes model =
    { model
        | nodes =
            model.rules
                |> List.map
                    (\r ->
                        let
                            inTemplate =
                                if String.contains r.pattern model.template then
                                    1

                                else
                                    0
                        in
                        ( r.pattern
                        , { left = String.left 1 r.pattern ++ r.replacement
                          , right = r.replacement ++ String.right 1 r.pattern
                          , count = inTemplate
                          , level = inTemplate
                          }
                        )
                    )
                |> Dict.fromList
    }


parse : List String -> Model
parse input =
    input
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( idx, str ) m ->
                if idx == 0 then
                    { m | template = str }

                else
                    { m | rules = (parseRule str :: List.map Just m.rules) |> List.filterMap identity }
            )
            (Model "" [] Dict.empty)
        |> createNodes


parseRule : String -> Maybe Rule
parseRule str =
    case String.split " -> " str of
        [ left, right ] ->
            Rule left right |> Just

        _ ->
            Nothing
