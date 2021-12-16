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


solveA : List String -> Int
solveA strs =
    strs
        |> List.map
            (\s ->
                toBits s
                    |> parsePacket
                    |> Tuple.first
                    |> sumVersions
            )
        |> List.sum


solveB : List String -> Int
solveB strs =
    strs
        |> String.join ""
        |> toBits
        |> parsePacket
        |> Tuple.first
        |> calc


calc : Packet -> Int
calc p =
    case p of
        Literal _ _ { nr } ->
            nr

        Operator _ id _ subPacks ->
            case id of
                0 ->
                    subPacks |> List.map calc |> List.sum

                1 ->
                    subPacks |> List.map calc |> List.product

                2 ->
                    subPacks |> List.map calc |> List.minimum |> Maybe.withDefault 0

                3 ->
                    subPacks |> List.map calc |> List.maximum |> Maybe.withDefault 0

                5 ->
                    case subPacks of
                        [ first, second ] ->
                            if calc first > calc second then
                                1

                            else
                                0

                        _ ->
                            Debug.todo "5"

                6 ->
                    case subPacks of
                        [ first, second ] ->
                            if calc first < calc second then
                                1

                            else
                                0

                        _ ->
                            Debug.todo "6"

                7 ->
                    case subPacks of
                        [ first, second ] ->
                            if calc first == calc second then
                                1

                            else
                                0

                        _ ->
                            Debug.todo "7"

                _ ->
                    Debug.todo "cannot calc"


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
            parseInt 3 bools

        ( id, rest ) =
            parseInt 3 r1
    in
    case id of
        4 ->
            parseLiteralPacket rest version id

        _ ->
            parseOperatorPacket rest version id


parseLiteralPacket : List Bool -> Int -> Int -> ( Packet, List Bool )
parseLiteralPacket rest version id =
    let
        ( nr, r2 ) =
            parserLiteralChunks rest Binary.empty
    in
    ( Literal version id { nr = nr }, r2 )


parseOperatorPacket : List Bool -> Int -> Int -> ( Packet, List Bool )
parseOperatorPacket rest version id =
    let
        ( ltid, r3 ) =
            parseInt 1 rest

        ( amt, r4 ) =
            case ltid of
                0 ->
                    parseInt 15 r3

                1 ->
                    parseInt 11 r3

                _ ->
                    Debug.todo "boom"
    in
    if ltid == 0 then
        let
            ( sp, _ ) =
                parseSubPackets0 (List.take amt r4) []
        in
        ( Operator version id { lengthTypeId = ltid, subLen = amt } (List.reverse sp), List.drop amt r4 )

    else
        let
            ( sp, r5 ) =
                parseSubPackets1 amt r4 []
        in
        ( Operator version id { lengthTypeId = ltid, subLen = amt } (List.reverse sp), r5 )


parseSubPackets0 : List Bool -> List Packet -> ( List Packet, List Bool )
parseSubPackets0 bools packets =
    if List.isEmpty bools then
        ( packets, bools )

    else
        let
            ( p, rest ) =
                parsePacket bools
        in
        parseSubPackets0 rest (p :: packets)


parseSubPackets1 : Int -> List Bool -> List Packet -> ( List Packet, List Bool )
parseSubPackets1 remaining bools packets =
    if remaining == 0 then
        ( packets, bools )

    else
        let
            ( p, rest ) =
                parsePacket bools
        in
        parseSubPackets1 (remaining - 1) rest (p :: packets)


parserLiteralChunks : List Bool -> Bits -> ( Int, List Bool )
parserLiteralChunks bools bits =
    let
        chunk =
            bools |> List.take 5

        rest =
            bools |> List.drop 5

        rem =
            Binary.append bits (List.drop 1 chunk |> Binary.fromBooleans)
    in
    case List.head chunk of
        Just False ->
            ( rem |> Binary.toDecimal, rest )

        Just True ->
            parserLiteralChunks rest rem

        _ ->
            Debug.todo "dontknow"


parseInt : Int -> List Bool -> ( Int, List Bool )
parseInt amt bools =
    ( bools
        |> List.take amt
        |> Binary.fromBooleans
        |> Binary.toDecimal
    , bools
        |> List.drop amt
    )


toBits : String -> List Bool
toBits str =
    Binary.fromHex str
        |> Binary.toBooleans


parse : List String -> List String
parse input =
    input
