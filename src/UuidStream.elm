module UuidStream
    exposing
        ( UuidStream
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

@docs UuidStream


## Stream Generators

@docs uuidStream, uuidStringStream


## Stream Consumers

@docs consume, consumeMany

-}

import InfiniteStream as Stream exposing (Stream)
import Random.Pcg.Extended as Random exposing (Generator, Seed)
import Uuid exposing (Uuid)


mkStream : Generator a -> Seed -> Stream a
mkStream generator seed =
    let
        ( item, newSeed ) =
            Random.step generator seed
    in
        Stream.stream item (\() -> mkStream generator newSeed)


{-| An infinite stream of UUIDs.
-}
type UuidStream uuid
    = UuidStream (Stream uuid)


{-| Create an infinite stream of [Uuids](http://package.elm-lang.org/packages/Zinggi/elm-uuid/1.0.0/Uuid#Uuid) from integer seeds. It's best to
generate these integers via JavaScript with `window.crypto.getRandomValues(..)`. More on that [here](http://package.elm-lang.org/packages/Zinggi/elm-random-pcg-extended/2.1.0/Random-Pcg-Extended#initialSeed).
-}
uuidStream : Int -> List Int -> UuidStream Uuid
uuidStream seed seedExtension =
    UuidStream (mkStream Uuid.generator (Random.initialSeed seed seedExtension))


{-| The same as `uuidStream` but returns a stream of UUIDs as `String`s.
The strings are in the canonical 8-4-4-4-12 form (e.g. "8e1c60e6-8860-48d6-82ee-cc894810490d").
-}
uuidStringStream : Int -> List Int -> UuidStream String
uuidStringStream seed seedExtension =
    UuidStream (mkStream Uuid.stringGenerator (Random.initialSeed seed seedExtension))


{-| Consume an element of a stream, producing a tuple with the next value and a new stream.
-}
consume : UuidStream uuid -> ( uuid, UuidStream uuid )
consume (UuidStream stream) =
    let
        ( uuid, newStream ) =
            Stream.consume stream
    in
        ( uuid, UuidStream newStream )


{-| Consume multiple elements of a stream, producing a tuple with a list of values and a new stream.
-}
consumeMany : Int -> UuidStream uuid -> ( List uuid, UuidStream uuid )
consumeMany n (UuidStream stream) =
    let
        ( uuids, newStream ) =
            Stream.consumeMany n stream
    in
        ( uuids, UuidStream newStream )
