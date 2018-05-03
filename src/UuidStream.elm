module UuidStream
    exposing
        ( Stream
        , UuidStream
        , UuidStringStream
        , consume
        , consumeMany
        , uuidStream
        , uuidStringStream
        )

{-| This module provides an easy way to generate infinite unique identifiers without
having to keep track of random seeds. Each time you consume a Uuid from the stream,
the next Uuid will be automatically generated.

The actual Uuid generation is handled by [Zinggi's Uuid library](http://package.elm-lang.org/packages/Zinggi/elm-uuid/1.0.0/Uuid). I suggest you take a
look at the documentation for that as it explains how to properly seed the generator so
you do not end up with duplicate Uuids!

@docs Stream

## Stream Generators

@docs UuidStream, uuidStream, UuidStringStream, uuidStringStream

## Stream Consumers

@docs consume, consumeMany
-}

import Random.Pcg.Extended as Random exposing (Generator, Seed)
import Uuid exposing (Uuid)

{-| A general stream type.
-}
type Stream a
    = Cons a (() -> Stream a)


mkStream : Generator a -> Seed -> Stream a
mkStream generator seed =
    let
        ( item, newSeed ) =
            Random.step generator seed
    in
        Cons item (\() -> mkStream generator newSeed)

{-| A specialized stream type that contains Uuids.
-}
type alias UuidStream =
    Stream Uuid

{-| Create a stream of Uuids from random integers. These are the same integers
you would provide to the `initialSeed` function from [Random.Pcg.Extended](http://package.elm-lang.org/packages/Zinggi/elm-random-pcg-extended/2.1.0/Random-Pcg-Extended#initialSeed) and
should be generated out in JavaScript-land with `window.crypto.getRandomValues(..)`.
-}
uuidStream : Int -> List Int -> UuidStream
uuidStream seed seedExtension =
    mkStream Uuid.generator (Random.initialSeed seed seedExtension)

{-| A specialized stream type that contains Uuids as Strings in the canonical 8-4-4-4-12 form.
-}
type alias UuidStringStream =
    Stream String

{-| The same as `uuidStream` but returns a `UuidStringStream` instead of a `UuidStream`.
-}
uuidStringStream : Int -> List Int -> UuidStringStream
uuidStringStream seed seedExtension =
    mkStream Uuid.stringGenerator (Random.initialSeed seed seedExtension)

{-| Consume an element of a stream, producing a tuple with the next value and a new stream.
-}
consume : Stream a -> ( a, Stream a )
consume (Cons value next) =
    ( value, next () )

{-| Consume multiple elements of a stream, producing a tuple with a list of values and a new stream.
-}
consumeMany : Int -> Stream a -> ( List a, Stream a )
consumeMany numToConsume stream =
    let
        go n ( acc, currentStream ) =
            if n > 0 then
                let
                    ( value, newStream ) =
                        consume currentStream
                in
                    go (n - 1) ( value :: acc, newStream )
            else
                ( List.reverse acc, currentStream )
    in
        go numToConsume ( [], stream )
