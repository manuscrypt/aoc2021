module Day16 exposing (..)

import Binary exposing (Bits)
import List.Extra as List
import Posix.IO as IO exposing (IO, Process)
import Utils exposing (..)


type Packet
    = Literal Int Int { nr : Int }
    | Operator Int Int { lengthTypeId : Int, subLen : Int } (List Packet)


program : Process -> IO ()
program _ =
    let
        input =
            getInput "d16" "input"

        sample =
            getInput "d16" "sample"

        xx =
            solveA [ "8A004A801A8002F478" ]
    in
    -- IO.do
    --     (IO.combine
    --         [ sample
    --             |> IO.map (parse >> solveA)
    --             |> IO.andThen (output "Sample Part A: ")
    --         , sample
    --             |> IO.map (parse >> solveB)
    --             |> IO.andThen (output "Sample Part B: ")
    --         , input
    --             |> IO.map (parse >> solveA)
    --             |> IO.andThen (output "Part A: ")
    --         , input
    --             |> IO.map (parse >> solveB)
    --             |> IO.andThen (output "Part B: ")
    --         ]
    --     )
    IO.return ()


solveA : List String -> Int
solveA strs =
    strs |> List.map (\s -> toBits s |> Debug.log "bits" |> parsePacket |> Debug.log "packets" |> Tuple.first |> sumVersions |> Debug.log s) |> List.sum


solveB : a -> Int
solveB _ =
    0


sumVersions : Packet -> Int
sumVersions p =
    case p of
        Literal v _ _ ->
            v

        Operator v _ _ subs ->
            v + List.foldl (\sp i -> i + sumVersions sp) 0 subs


parsePacket : List Bool -> ( Packet, List Bool )
parsePacket bools =
    let
        ( version, r1 ) =
            parseVersion bools

        ( id, rest ) =
            parseId r1

        xx =
            rest |> Binary.fromBooleans |> Binary.toIntegers |> Debug.log "rest"
    in
    case id of
        4 ->
            parseLiteralPacket rest version id

        _ ->
            parseOperatorPacket rest version id


parseOperatorPacket : List Bool -> Int -> Int -> ( Packet, List Bool )
parseOperatorPacket rest version id =
    let
        ( ltid, r3 ) =
            parseOperatorLengthTypeId rest

        ( amt, r4 ) =
            case ltid of
                0 ->
                    ( List.take 15 r3 |> Binary.fromBooleans |> Binary.toDecimal, List.drop 15 r3 )

                1 ->
                    ( List.take 11 r3 |> Binary.fromBooleans |> Binary.toDecimal, List.drop 11 r3 )

                _ ->
                    Debug.todo "boom"

        ( subPacks, r5 ) =
            if ltid == 0 then
                parseSubPackets0 amt r4 []

            else
                parseSubPackets1 amt r4 []
    in
    ( Operator version id { lengthTypeId = ltid, subLen = amt } (List.reverse subPacks), r5 )


parseLiteralPacket : List Bool -> Int -> Int -> ( Packet, List Bool )
parseLiteralPacket rest version id =
    let
        ( nr, r2 ) =
            parserLiteralChunks rest Binary.empty
    in
    ( Literal version id { nr = nr }, r2 )


parseSubPackets0 : Int -> List Bool -> List Packet -> ( List Packet, List Bool )
parseSubPackets0 remaining bools packets =
    if remaining <= 0 then
        ( packets, bools )

    else
        let
            ( p, rest ) =
                parsePacket (List.take remaining bools)
        in
        parseSubPackets0 (List.length rest) rest (p :: packets)


parseSubPackets1 : Int -> List Bool -> List Packet -> ( List Packet, List Bool )
parseSubPackets1 remaining bools packets =
    if remaining == 0 then
        ( packets, [] )

    else
        let
            ( p, rest ) =
                parsePacket bools
        in
        parseSubPackets1 (remaining - 1) rest (p :: packets)


parseOperatorLengthTypeId : List Bool -> ( Int, List Bool )
parseOperatorLengthTypeId bools =
    if List.length bools < 1 then
        Debug.todo "cannot parse oplenid"

    else
        ( List.take 1 bools |> Binary.fromBooleans |> Binary.toDecimal, List.drop 1 bools )


parserLiteralChunks : List Bool -> Bits -> ( Int, List Bool )
parserLiteralChunks bools bits =
    let
        chunk =
            bools |> List.take 5

        rest =
            bools |> List.drop 5
    in
    if List.length chunk /= 5 then
        ( Binary.toDecimal bits, bools )

    else
        let
            nr =
                List.drop 1 chunk |> Binary.fromBooleans

            rem =
                Binary.append bits nr
        in
        case List.head chunk of
            Just False ->
                ( rem |> Binary.toDecimal, rest )

            Just True ->
                parserLiteralChunks rest rem

            _ ->
                Debug.todo "dontknow"



--( bits |> Binary.toDecimal, List.drop 5 bools )


parseId : List Bool -> ( Int, List Bool )
parseId bools =
    if List.length bools < 3 then
        Debug.todo "cannot parse id"

    else
        ( bools |> List.take 3 |> Binary.fromBooleans |> Binary.toDecimal, bools |> List.drop 3 )


parseVersion : List Bool -> ( Int, List Bool )
parseVersion bools =
    if List.length bools < 3 then
        Debug.todo "cannot parse version"

    else
        ( bools |> List.take 3 |> Binary.fromBooleans |> Binary.toDecimal, bools |> List.drop 3 )


toBits : String -> List Bool
toBits str =
    Binary.fromHex str |> Binary.toBooleans


parse : List String -> List String
parse input =
    input
