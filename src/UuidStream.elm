module UuidStream
    exposing
        ( UuidStream
        , UuidStringStream
        , consume
        , consumeMany
        , uuidStream
        , uuidStringStream
        )

{-| This module provides an easy way to generate infinite unique identifiers without
having to keep track of random seeds. Each time you consume a UUID from the stream,
the next UUID will be automatically generated.

The actual UUID generation is handled by [Zinggi's Uuid library](http://package.elm-lang.org/packages/Zinggi/elm-uuid/1.0.0/Uuid). I suggest you take a
look at the documentation for that as it explains how to properly seed the generator so
you don't end up with duplicate UUIDs!


## Stream Generators

@docs uuidStream, uuidStringStream


## Stream Types

@docs UuidStream, UuidStringStream


## Stream Consumers

@docs consume, consumeMany

-}

import Random.Pcg.Extended as Random exposing (Generator, Seed)
import Uuid exposing (Uuid)


type Stream a
    = Cons a (() -> Stream a)


mkStream : Generator a -> Seed -> Stream a
mkStream generator seed =
    let
        ( item, newSeed ) =
            Random.step generator seed
    in
        Cons item (\() -> mkStream generator newSeed)


{-| An infinite stream of [Uuids](http://package.elm-lang.org/packages/Zinggi/elm-uuid/1.0.0/Uuid#Uuid).
-}
type alias UuidStream =
    Stream Uuid


{-| Create an infinite stream of [Uuids](http://package.elm-lang.org/packages/Zinggi/elm-uuid/1.0.0/Uuid#Uuid) from integer seeds. It's best to
generate these integers via JavaScript with `window.crypto.getRandomValues(..)`. More on that [here](http://package.elm-lang.org/packages/Zinggi/elm-random-pcg-extended/2.1.0/Random-Pcg-Extended#initialSeed).
-}
uuidStream : Int -> List Int -> UuidStream
uuidStream seed seedExtension =
    mkStream Uuid.generator (Random.initialSeed seed seedExtension)


{-| An infinite stream of UUID Strings in the canonical 8-4-4-4-12 form (e.g. "8e1c60e6-8860-48d6-82ee-cc894810490d").
-}
type alias UuidStringStream =
    Stream String


{-| The same as `uuidStream` but returns a `UuidStringStream`.
-}
uuidStringStream : Int -> List Int -> UuidStringStream
uuidStringStream seed seedExtension =
    mkStream Uuid.stringGenerator (Random.initialSeed seed seedExtension)


{-| Consume an element of a stream, producing a tuple with the next value and a new stream.
-}
consume : Stream uuid -> ( uuid, Stream uuid )
consume (Cons value next) =
    ( value, next () )


{-| Consume multiple elements of a stream, producing a tuple with a list of values and a new stream.
-}
consumeMany : Int -> Stream uuid -> ( List uuid, Stream uuid )
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
