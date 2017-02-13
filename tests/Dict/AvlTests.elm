module Dict.AvlTests exposing (..)

import Dict exposing (values)
import Dict.Avl as Avl
import Expect
import Fuzz
import Test exposing (..)


empty : Test
empty =
    describe "empty"
        [ test "isEmpty" <|
            \() ->
                Avl.empty
                    |> Avl.isEmpty
                    |> Expect.true "expected the empty set to be empty"
        , test "size" <|
            \() ->
                Avl.empty
                    |> Avl.size
                    |> Expect.equal 0
        ]


singleton : Test
singleton =
    describe "singleton"
        [ test "size" <|
            \() ->
                Avl.singleton 0 ()
                    |> Avl.size
                    |> Expect.equal 1
        ]


insert : Test
insert =
    describe "insert"
        [ test "into empty" <|
            \() ->
                Avl.empty
                    |> Avl.insert 0 ()
                    |> Avl.size
                    |> Expect.equal 1
        , fuzz2 Fuzz.string Fuzz.string "into nonempty" <|
            \a b ->
                Avl.singleton a ()
                    |> Avl.insert b ()
                    |> Avl.size
                    |> Expect.equal
                        (if a == b then
                            1
                         else
                            2
                        )
        , fuzz Fuzz.string "overwriting existing" <|
            \key ->
                Avl.singleton key 1
                    |> Avl.insert key 2
                    |> Avl.get key
                    |> Expect.equal (Just 2)
        ]


update : Test
update =
    describe "update"
        [ test "when key present" <|
            \() ->
                let
                    core =
                        Dict.singleton "a" 0
                            |> Dict.update "a" (Maybe.map <| (+) 1)
                            |> Dict.toList

                    avl =
                        Avl.singleton "a" 0
                            |> Avl.update "a" (Maybe.map <| (+) 1)
                            |> Avl.toList
                in
                    Expect.equal core avl
        , test "when key absent" <|
            \() ->
                let
                    core =
                        Dict.singleton "a" 0
                            |> Dict.update "b" (Maybe.map <| (+) 1)
                            |> Dict.toList

                    avl =
                        Avl.singleton "a" 0
                            |> Avl.update "b" (Maybe.map <| (+) 1)
                            |> Avl.toList
                in
                    Expect.equal core avl
        , test "when return value is Nothing" <|
            \() ->
                let
                    core =
                        Dict.singleton "a" 0
                            |> Dict.update "a" (always Nothing)
                            |> Dict.toList

                    avl =
                        Avl.singleton "a" 0
                            |> Avl.update "a" (always Nothing)
                            |> Avl.toList
                in
                    Expect.equal core avl
        ]


remove : Test
remove =
    describe "remove"
        [ test "empty" <|
            \() ->
                Avl.empty
                    |> Avl.remove 1
                    |> Expect.equal Avl.empty
        , fuzz Fuzz.string "singleton" <|
            \key ->
                Avl.singleton key ()
                    |> Avl.remove key
                    |> Expect.equal Avl.empty
        , fuzz (Fuzz.list Fuzz.string) "same as core" <|
            \ls ->
                let
                    values =
                        List.map (\s -> ( s, () )) ls

                    toRemove =
                        List.head ls |> Maybe.withDefault ""

                    expected =
                        Dict.fromList values
                            |> Dict.remove toRemove
                            |> Dict.toList

                    actual =
                        Avl.fromList values
                            |> Avl.remove toRemove
                            |> Avl.toList
                in
                    Expect.equal expected actual
        ]


member : Test
member =
    describe "member"
        [ fuzz Fuzz.int "true" <|
            \i ->
                Avl.singleton i ()
                    |> Avl.member i
                    |> Expect.true "expected a value to be a member of a singleton containing the value"
        , fuzz Fuzz.int "false" <|
            \i ->
                Avl.singleton (i + 1) ()
                    |> Avl.member i
                    |> Expect.false "expected a value not to be a member of a singleton that doesn't contain it"
        ]


get : Test
get =
    describe "get"
        [ fuzz Fuzz.int "is a member" <|
            \i ->
                Avl.singleton i ()
                    |> Avl.get i
                    |> Expect.equal (Just ())
        , fuzz Fuzz.int "is not a member" <|
            \i ->
                Avl.singleton (i + 1) ()
                    |> Avl.get i
                    |> Expect.equal Nothing
        ]


size : Test
size =
    describe "size"
        [ test "empty" <|
            \() ->
                Avl.empty
                    |> Avl.size
                    |> Expect.equal 0
        , fuzz (Fuzz.intRange 1 1000) "nonempty" <|
            \size ->
                List.range 1 size
                    |> List.map (flip (,) ())
                    |> Avl.fromList
                    |> Avl.size
                    |> Expect.equal size
        ]


keys : Test
keys =
    describe "keys"
        [ fuzz (Fuzz.list Fuzz.string) "same as core" <|
            \rawKeys ->
                let
                    values =
                        List.map (\k -> ( k, () )) rawKeys

                    core =
                        Dict.fromList values
                            |> Dict.keys

                    avl =
                        Avl.fromList values
                            |> Avl.keys
                in
                    Expect.equal core avl
        ]


values : Test
values =
    describe "values"
        [ fuzz (Fuzz.list Fuzz.string) "same as core" <|
            \values ->
                let
                    pairs =
                        List.indexedMap (,) values

                    core =
                        Dict.fromList pairs
                            |> Dict.values

                    avl =
                        Avl.fromList pairs
                            |> Avl.values
                in
                    Expect.equal core avl
        ]


listOps : Test
listOps =
    fuzz (Fuzz.list <| Fuzz.tuple ( Fuzz.string, Fuzz.int )) "list roundtrip" <|
        \list ->
            let
                core =
                    Dict.fromList list |> Dict.toList

                avl =
                    Avl.fromList list |> Avl.toList
            in
                Expect.equal core avl


foldOps : Test
foldOps =
    describe "fold operations"
        [ test "foldr sum" <|
            \() ->
                [ 1, 2, 3, 4, 5 ]
                    |> List.map (flip (,) ())
                    |> Avl.fromList
                    |> Avl.foldr (\key _ total -> total * key) 1
                    |> Expect.equal 120
        , test "foldl sum" <|
            \() ->
                [ 1, 2, 3, 4, 5 ]
                    |> List.map (flip (,) ())
                    |> Avl.fromList
                    |> Avl.foldl (\key _ total -> total * key) 1
                    |> Expect.equal 120
        ]


filter : Test
filter =
    describe "filter"
        [ test "empty" <|
            \_ ->
                Avl.empty
                    |> Avl.filter (\_ _ -> False)
                    |> Expect.equal Avl.empty
        , fuzz Fuzz.int "matches" <|
            \key ->
                Avl.singleton key ()
                    |> Avl.filter (\k _ -> k == key)
                    |> Expect.equal (Avl.singleton key ())
        , fuzz Fuzz.int "does not match" <|
            \key ->
                Avl.singleton key ()
                    |> Avl.filter (\k _ -> k /= key)
                    |> Expect.equal Avl.empty
        ]


partition : Test
partition =
    describe "partition"
        [ test "empty" <|
            \_ ->
                Avl.empty
                    |> Avl.partition (\_ _ -> False)
                    |> Expect.equal ( Avl.empty, Avl.empty )
        , fuzz Fuzz.int "matches" <|
            \key ->
                Avl.singleton key ()
                    |> Avl.partition (\k _ -> k == key)
                    |> Expect.equal ( Avl.singleton key (), Avl.empty )
        , fuzz Fuzz.int "does not match" <|
            \key ->
                Avl.singleton key ()
                    |> Avl.partition (\k _ -> k /= key)
                    |> Expect.equal ( Avl.empty, Avl.singleton key () )
        ]


combine : Test
combine =
    let
        kvList =
            Fuzz.list <| Fuzz.tuple ( Fuzz.int, Fuzz.int )
    in
        concat
            [ describe "union"
                [ fuzz2 kvList kvList "same as core" <|
                    \kv1 kv2 ->
                        Expect.equal
                            (Dict.union (Dict.fromList kv1) (Dict.fromList kv2) |> Dict.toList)
                            (Avl.union (Avl.fromList kv1) (Avl.fromList kv2) |> Avl.toList)
                ]
            , describe "intersect"
                [ fuzz2 kvList kvList "same as core" <|
                    \kv1 kv2 ->
                        Expect.equal
                            (Dict.intersect (Dict.fromList kv1) (Dict.fromList kv2) |> Dict.toList)
                            (Avl.intersect (Avl.fromList kv1) (Avl.fromList kv2) |> Avl.toList)
                ]
            , describe "diff"
                [ fuzz2 kvList kvList "same as core" <|
                    \kv1 kv2 ->
                        Expect.equal
                            (Dict.diff (Dict.fromList kv1) (Dict.fromList kv2) |> Dict.toList)
                            (Avl.diff (Avl.fromList kv1) (Avl.fromList kv2) |> Avl.toList)
                ]
            , describe "merge" <|
                let
                    discard : comparable -> a -> result -> result
                    discard k v acc =
                        acc

                    discardBoth : comparable -> a -> b -> result -> result
                    discardBoth k v1 v2 acc =
                        acc
                in
                    [ test "keep left" <|
                        \() ->
                            Avl.merge
                                Avl.insert
                                discardBoth
                                discard
                                (Avl.singleton 1 1)
                                (Avl.singleton 2 1)
                                Avl.empty
                                |> Expect.equal (Avl.singleton 1 1)
                    , test "keep right" <|
                        \() ->
                            Avl.merge
                                discard
                                discardBoth
                                Avl.insert
                                (Avl.singleton 1 1)
                                (Avl.singleton 2 1)
                                Avl.empty
                                |> Expect.equal (Avl.singleton 2 1)
                    , test "add both" <|
                        \() ->
                            Avl.merge
                                discard
                                (\k v1 v2 acc -> Avl.insert k (v1 + v2) acc)
                                discard
                                (Avl.singleton 1 1)
                                (Avl.singleton 1 1)
                                Avl.empty
                                |> Expect.equal (Avl.singleton 1 2)
                    ]
            ]


all : Test
all =
    concat
        [ empty
        , singleton
        , insert
        , update
        , remove
        , member
        , get
        , size
        , keys
        , values
        , listOps
        , foldOps
        , filter
        , partition
        , combine
        ]
