# Elm Uuid Stream

This module provides an easy way to generate infinite unique identifiers without having to keep track of random seeds. Each time you consume a UUID from the stream, the next UUID will be automatically generated.

The actual UUID generation is handled by [Zinggi's Uuid library](http://package.elm-lang.org/packages/Zinggi/elm-uuid/1.0.0/Uuid). I suggest you take a look at the documentation for that as it explains how to properly seed the generator so you don't end up with duplicate UUIDs!