module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode
import Json.Encode
import PhotoGroove
import Test exposing (..)


fuzzTest =
    fuzz3 string int string "happy path" <|
        \url size title ->
            [ ( "url", Json.Encode.string url ), ( "size", Json.Encode.int size ), ( "title", Json.Encode.string title ) ]
                |> Json.Encode.object
                |> Json.Decode.decodeValue PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { url = url, size = size, title = title })


titleDefaultTest =
    test "title should default to (untitled)" <|
        \_ ->
            """ {"url": "http://fruits.com/apple.jpeg", "size": 5}"""
                |> Json.Decode.decodeString PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { url = "http://fruits.com/apple.jpeg", size = 5, title = "(untitled)" })
