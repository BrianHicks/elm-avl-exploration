module Dict.AvlTests exposing (..)

import Dict
import Dict.Avl as Avl
import Expect
import Fuzz
import Test exposing (..)


dropIn : Test
dropIn =
    describe "Dict and Dict.Avl yield the same results"
        [ fuzz Fuzz.string "member" <|
            \key ->
                let
                    core =
                        Dict.singleton key 0 |> Dict.member key

                    avl =
                        Avl.singleton key 0 |> Avl.member key
                in
                    Expect.equal core avl
        , fuzz2 Fuzz.string Fuzz.int "get" <|
            \key value ->
                let
                    core =
                        Dict.singleton key value |> Dict.get key

                    avl =
                        Avl.singleton key value |> Avl.get key
                in
                    Expect.equal core avl
        , fuzz (Fuzz.list <| Fuzz.tuple ( Fuzz.string, Fuzz.int )) "list roundtrip" <|
            \list ->
                let
                    core =
                        Dict.fromList list |> Dict.toList

                    avl =
                        Avl.fromList list |> Avl.toList
                in
                    Expect.equal core avl
        , fuzz (Fuzz.list <| Fuzz.tuple ( Fuzz.string, Fuzz.int )) "count" <|
            \list ->
                let
                    core =
                        Dict.fromList list |> Dict.size

                    avl =
                        Avl.fromList list |> Avl.size
                in
                    Expect.equal core avl
        ]


all : Test
all =
    concat [ dropIn ]
