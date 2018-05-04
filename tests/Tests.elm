module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set
import Test exposing (..)
import UuidStream exposing (..)


suite : Test
suite =
    describe "Elm UuidStream tests"
        [ mkTest "uuidStream" uuidStream seedFuzzer
        , mkTest "uuidStringStream" uuidStringStream seedFuzzer
        ]


seedFuzzer : Fuzzer ( Int, List Int )
seedFuzzer =
    Fuzz.map2 (,) Fuzz.int (Fuzz.list Fuzz.int)


mkTest label mkStream fuzzer =
    describe ("Stream tests for " ++ label)
        [ fuzz fuzzer "Repeating `consume` n times should produce the same results as `consumeMany s`" <|
            \( fuzzSeed, fuzzSeedExtension ) ->
                let
                    stream =
                        mkStream fuzzSeed fuzzSeedExtension

                    ( v1, s1 ) =
                        consume stream

                    ( v2, s2 ) =
                        consume s1

                    ( v3, s3 ) =
                        consume s2

                    ( v4, s4 ) =
                        consume s3

                    ( v5, s5 ) =
                        consume s4
                in
                    [ v1, v2, v3, v4, v5 ]
                        |> Expect.equal (Tuple.first <| consumeMany 5 stream)
        , fuzz fuzzer "The stream should not create duplicate Uuids" <|
            \( fuzzSeed, fuzzSeedExtension ) ->
                let
                    stream =
                        mkStream fuzzSeed fuzzSeedExtension

                    ( values, _ ) =
                        consumeMany 1000 stream

                    hasDuplicates =
                        List.foldl
                            (\v ( acc, set ) ->
                                let
                                    val =
                                        toString v
                                in
                                    if Set.member val set then
                                        ( True, set )

                                    else
                                        ( acc, Set.insert val set )
                            )
                            ( False, Set.empty )
                            values
                            |> Tuple.first
                in
                    Expect.false "Expected no duplicate Uuids" hasDuplicates
        ]
