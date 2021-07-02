module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode
import PhotoGroove
import Test exposing (..)


decoderTest : Test
decoderTest =
    test "happy path"
        (\_ ->
            """ {"url": "http://fruits.com/apple.jpeg", "size": 5, "title": "this is not an apple"}"""
                |> Json.Decode.decodeString PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { url = "http://fruits.com/apple.jpeg", size = 5, title = "this is not an apple" })
        )
