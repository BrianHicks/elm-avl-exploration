module Dict.Avl
    exposing
        ( Dict
        , empty
        , singleton
        , insert
        , update
        , remove
        , isEmpty
        , member
        , get
        , size
        , keys
        , values
        , toList
        , fromList
        , map
        , foldl
        , foldr
        , filter
        , partition
        , union
        , intersect
        , diff
        , merge
        )

{-| A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.

Insert, remove, and query operations all take *O(log n)* time.

# Dictionaries
@docs Dict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
@docs union, intersect, diff, merge
-}


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type Dict k v
    = Dict Int k v (Dict k v) (Dict k v)
    | Singleton k v
    | Empty



-- build


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Empty


{-| Create a dictionary with one key-value pair.
-}
singleton : comparable -> v -> Dict comparable v
singleton key value =
    Singleton key value


dict : comparable -> v -> Dict comparable v -> Dict comparable v -> Dict comparable v
dict key value left right =
    Dict
        (max (height left) (height right) |> (+) 1)
        key
        value
        left
        right


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert newKey newValue set =
    case set of
        Empty ->
            singleton newKey newValue

        Singleton key value ->
            if newKey == key then
                Singleton key newValue
            else if newKey < key then
                Dict 2 key value (Singleton newKey newValue) Empty
            else
                Dict 2 key value Empty (Singleton newKey newValue)

        Dict bal key value left right ->
            if newKey == key then
                Dict bal key newValue left right
            else if newKey < key then
                dict key value (insert newKey newValue left) right |> balance
            else
                dict key value left (insert newKey newValue right) |> balance


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update key updater source =
    source
        |> get key
        |> updater
        |> Maybe.map (\value -> insert key value source)
        |> Maybe.withDefault (remove key source)


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove item set =
    case set of
        Empty ->
            set

        Singleton key _ ->
            if item == key then
                Empty
            else
                set

        Dict _ key value left right ->
            if item < key then
                dict key value (remove item left) right |> balance
            else if item > key then
                dict key value left (remove item right) |> balance
            else
                foldl insert left right



-- query


{-| Determine if a dictionary is empty.
    isEmpty empty == True
-}
isEmpty : Dict k v -> Bool
isEmpty set =
    case set of
        Empty ->
            True

        _ ->
            False


{-| Determine if a key is in a dictionary.
-}
member : comparable -> Dict comparable v -> Bool
member item set =
    get item set /= Nothing


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing
-}
get : comparable -> Dict comparable v -> Maybe v
get item set =
    case set of
        Empty ->
            Nothing

        Singleton key value ->
            if item == key then
                Just value
            else
                Nothing

        Dict _ key value left right ->
            if item < key then
                get item left
            else if item > key then
                get item right
            else
                Just value


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict comparable v -> Int
size =
    foldl (\_ _ acc -> acc + 1) 0


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
    keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]
-}
keys : Dict comparable v -> List comparable
keys =
    toList >> List.map Tuple.first


{-| Get all of the values in a dictionary, in the order of their keys.
    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
-}
values : Dict comparable v -> List v
values =
    toList >> List.map Tuple.second


height : Dict comparable v -> Int
height set =
    case set of
        Empty ->
            0

        Singleton _ _ ->
            1

        Dict height _ _ _ _ ->
            height


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable, v ) -> Dict comparable v
fromList items =
    List.foldl (uncurry insert) empty items


{-| Convert a dictionary into an association list of key-value pairs, sorted by
keys.
-}
toList : Dict comparable v -> List ( comparable, v )
toList =
    foldr
        (\key value list ->
            ( key, value ) :: list
        )
        []



-- transform


{-| Apply a function to all values in a dictionary.
-}
map : (comparable -> a -> b) -> Dict comparable a -> Dict comparable b
map fn set =
    foldl
        (\key value acc -> insert key (fn key value) acc)
        empty
        set


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
foldl fn acc set =
    case set of
        Empty ->
            acc

        Singleton key value ->
            fn key value acc

        Dict _ key value left right ->
            let
                accLeft =
                    foldl fn acc left

                accHead =
                    fn key value accLeft

                accRight =
                    foldl fn accHead right
            in
                accRight


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
foldr fn acc set =
    case set of
        Empty ->
            acc

        Singleton key value ->
            fn key value acc

        Dict _ key value left right ->
            let
                accRight =
                    foldr fn acc right

                accHead =
                    fn key value accRight

                accLeft =
                    foldr fn accHead left
            in
                accLeft


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
filter cmp set =
    foldl
        (\key value acc ->
            if cmp key value then
                insert key value acc
            else
                acc
        )
        empty
        set


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (comparable -> v -> Bool) -> Dict comparable v -> ( Dict comparable v, Dict comparable v )
partition cmp set =
    foldl
        (\key value ( yes, no ) ->
            if cmp key value then
                ( insert key value yes, no )
            else
                ( yes, insert key value no )
        )
        ( empty, empty )
        set



-- combine


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict comparable v -> Dict comparable v -> Dict comparable v
union d1 d2 =
    foldl insert d2 d1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect d1 d2 =
    filter (\key _ -> member key d2) d1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict comparable v -> Dict comparable v -> Dict comparable v
diff =
    foldl (\key _ acc -> remove key acc)


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

  1. Only in the left dictionary.
  2. In both dictionaries.
  3. Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.
-}
merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Dict comparable a
    -> Dict comparable b
    -> result
    -> result
merge leftStep bothStep rightStep left right acc =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        stepState rKey rValue ( rest, leftStep lKey lValue result )
                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )
                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList left, acc ) right
    in
        List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- rebalancing


balance : Dict comparable v -> Dict comparable v
balance set =
    case set of
        Empty ->
            set

        Singleton _ _ ->
            set

        Dict _ key value left right ->
            let
                setDiff =
                    heightDiff set
            in
                if setDiff < -1 then
                    if heightDiff left == 1 then
                        -- left leaning tree with right-leaning left subtree. Rotate left, then right.
                        dict key value (rotl left) right |> rotr
                    else
                        -- left leaning tree, generally. Rotate right.
                        rotr set
                else if setDiff > 1 then
                    if heightDiff right == -1 then
                        -- right leaning tree with left-leaning right subtree. Rotate right, then left.
                        dict key value left (rotr right) |> rotl
                    else
                        -- right leaning tree, generally. Rotate left.
                        rotl set
                else
                    -- diff is -1, 0, or 1. Already balanced, no operation required.
                    set


heightDiff : Dict comparable v -> Int
heightDiff set =
    case set of
        Empty ->
            0

        Singleton _ _ ->
            0

        Dict _ _ _ left right ->
            height right - height left


balanceSubtrees : Dict comparable v -> Dict comparable v
balanceSubtrees set =
    case set of
        Empty ->
            set

        Singleton _ _ ->
            set

        Dict _ key value left right ->
            dict key value (balance left) (balance right)


rotl : Dict comparable v -> Dict comparable v
rotl set =
    case set of
        Dict _ key value lessThans (Dict _ subKey subValue betweens greaterThans) ->
            dict subKey subValue (dict key value lessThans betweens) greaterThans

        _ ->
            set


rotr : Dict comparable v -> Dict comparable v
rotr set =
    case set of
        Dict _ key value (Dict _ subKey subValue lessThans betweens) greaterThans ->
            dict subKey subValue lessThans (dict key value betweens greaterThans)

        _ ->
            set
