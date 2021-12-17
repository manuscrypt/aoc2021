module Day17 exposing (..)

import Binary exposing (Bits)
import Browser
import List.Extra as List
import Math.Vector2 exposing (Vec2, add, dot, fromRecord, getX, getY, toRecord, vec2)
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type alias Probe =
    { pos : Vec2
    , vel : Vec2
    , hit : Bool
    , trajectory : List Vec2
    , overshot : Vec2
    }


type alias Target =
    { xAxis : Vec2, yAxis : Vec2 }


targetSample =
    { xAxis = vec2 20 30
    , yAxis = vec2 -10 -5
    }


targetInput =
    { xAxis = vec2 124 174
    , yAxis = vec2 -123 -86
    }


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d16" "input"

        sample =
            getInput "d16" "sample"
    in
    IO.do
        (IO.combine
            [ output "Sample Part A: " (solveA targetSample)
            , output "Part A: " (solveA targetInput)
            , output "Sample Part B: " (solveB targetSample)
            , output "Part B: " (solveB targetInput)
            ]
        )
        return


solveA : Target -> Int
solveA target =
    runProbes target
        |> List.filter (\p -> p.hit)
        |> List.map (\p -> p.trajectory |> List.maximumBy getY |> Maybe.withDefault (vec2 -9999 -9999))
        |> List.map getY
        |> List.maximum
        |> Maybe.withDefault -9999
        |> truncate


solveB : Target -> Int
solveB target =
    runProbes target
        |> List.filter (\p -> p.hit)
        |> List.length


runProbes : Target -> List Probe
runProbes target =
    let
        initX =
            List.range 0 300

        initY =
            List.range (getX target.yAxis |> truncate) 300

        vels =
            initX |> List.concatMap (\x -> initY |> List.map (\y -> vec2 (toFloat x) (toFloat y)))

        probes =
            vels |> List.map initProbe

        results =
            probes |> List.map (stepUntilOvershotOrHit target)
    in
    results


initProbe : Vec2 -> Probe
initProbe vel =
    { pos = vec2 0 0
    , vel = vel
    , hit = False
    , trajectory = []
    , overshot = vec2 0 0
    }


stepUntilOvershotOrHit : Target -> Probe -> Probe
stepUntilOvershotOrHit t p =
    let
        hitTarget =
            isInTarget t p

        overshot =
            overshotTarget t p |> toRecord
    in
    if hitTarget || overshot.x < 0 || overshot.y > 0 then
        { p | hit = hitTarget, overshot = overshot |> fromRecord }

    else
        stepUntilOvershotOrHit t (step p)


step : Probe -> Probe
step probe =
    let
        p0 =
            { probe | pos = add probe.vel probe.pos, trajectory = probe.pos :: probe.trajectory }

        p1 =
            { p0 | vel = add (drag p0.vel) p0.vel }
    in
    p1


isInTarget : Target -> Probe -> Bool
isInTarget target p =
    let
        { x, y } =
            toRecord p.pos

        xAxis =
            toRecord target.xAxis

        yAxis =
            toRecord target.yAxis
    in
    x >= xAxis.x && x <= xAxis.y && y >= yAxis.x && y <= yAxis.y


overshotTarget : Target -> Probe -> Vec2
overshotTarget t p =
    let
        { x, y } =
            toRecord p.pos

        maxX =
            getY t.xAxis

        maxY =
            getX t.yAxis
    in
    vec2 (maxX - x) (maxY - y)


drag : Vec2 -> Vec2
drag vel =
    let
        { x, y } =
            toRecord vel
    in
    if x > 0 then
        vec2 -1 -1

    else if x < 0 then
        vec2 1 -1

    else
        vec2 0 -1
