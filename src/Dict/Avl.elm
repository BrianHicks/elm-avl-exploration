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
          -- , keys
          -- , values
        , toList
        , fromList
        , map
        , foldl
        , foldr
        , filter
        , partition
          -- , union
          -- , intersect
          -- , diff
          -- , merge
        )

{-| A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes Int, Float, Time, Char, String, and tuples or lists of
comparable types.

Insert, remove, and query operations all take *O(log n)* time.

# Dictionaries
@docs Dict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

# Lists
@docs toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
-}


{-| TODO document
-}
type Dict k v
    = Dict Int k v (Dict k v) (Dict k v)
    | Empty



-- build


{-| TODO document
-}
empty : Dict k v
empty =
    Empty


{-| TODO document
-}
singleton : comparable -> v -> Dict comparable v
singleton key value =
    Dict 1 key value empty empty


dict : comparable -> v -> Dict comparable v -> Dict comparable v -> Dict comparable v
dict key value left right =
    Dict
        (max (height left) (height right) |> (+) 1)
        key
        value
        left
        right


{-| TODO document
-}
insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert newKey newValue set =
    case set of
        Empty ->
            singleton newKey newValue

        Dict bal key value left right ->
            if newKey < key then
                dict key value (insert newKey newValue left) right |> balance
            else if newKey > key then
                dict key value left (insert newKey newValue right) |> balance
            else
                Dict bal key newValue left right


{-| TODO document
-}
update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update key updater source =
    source
        |> get key
        |> updater
        |> Maybe.map (\value -> insert key value source)
        |> Maybe.withDefault (remove key source)


{-| TODO document
-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove item set =
    case set of
        Empty ->
            set

        Dict _ key value left right ->
            if item < key then
                dict key value (remove item left) right |> balance
            else if item > key then
                dict key value left (remove item right) |> balance
            else
                foldl insert left right



-- query
-- TODO: iEmpty : Dict k v -> Bool


{-| TODO document
-}
isEmpty : Dict k v -> Bool
isEmpty set =
    case set of
        Empty ->
            True

        _ ->
            False


{-| TODO document
-}
member : comparable -> Dict comparable v -> Bool
member item set =
    get item set /= Nothing


{-| TODO document
-}
get : comparable -> Dict comparable v -> Maybe v
get item set =
    case set of
        Empty ->
            Nothing

        Dict _ key value left right ->
            if item < key then
                get item left
            else if item > key then
                get item right
            else
                Just value


{-| TODO document
-}
size : Dict comparable v -> Int
size =
    foldl (\_ _ acc -> acc + 1) 0


{-| TODO document
-}
keys : Dict comparable v -> List comparable
keys =
    toList >> List.map Tuple.first


{-| TODO document
-}
values : Dict comparable v -> List v
values =
    toList >> List.map Tuple.second


height : Dict comparable v -> Int
height set =
    case set of
        Empty ->
            0

        Dict height _ _ _ _ ->
            height


{-| TODO: docs
-}
fromList : List ( comparable, v ) -> Dict comparable v
fromList items =
    List.foldl (uncurry insert) empty items


{-| TODO: docs
-}
toList : Dict comparable v -> List ( comparable, v )
toList =
    foldr
        (\key value list ->
            ( key, value ) :: list
        )
        []



-- transform


{-| TODO: docs
-}
map : (comparable -> a -> b) -> Dict comparable a -> Dict comparable b
map fn set =
    foldl
        (\key value acc -> insert key (fn key value) acc)
        empty
        set


{-| TODO: docs
-}
foldl : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
foldl fn acc set =
    case set of
        Empty ->
            acc

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


{-| TODO: docs
-}
foldr : (comparable -> v -> b -> b) -> b -> Dict comparable v -> b
foldr fn acc set =
    case set of
        Empty ->
            acc

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


{-| TODO: docs
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


{-| TODO: docs
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



-- rebalancing


balance : Dict comparable v -> Dict comparable v
balance set =
    case set of
        Empty ->
            set

        Dict _ key value left right ->
            if heightDiff set == -2 && heightDiff left == 1 then
                -- left leaning tree with right-leaning left subtree. Rotate left, then right.
                dict key value (rotl left) right |> rotr
            else if heightDiff set < -1 then
                -- left leaning tree, generally. Rotate right.
                rotr set
            else if heightDiff set == 2 && heightDiff right == -1 then
                -- right leaning tree with left-leaning right subtree. Rotate right, then left.
                dict key value left (rotr right) |> rotl
            else if heightDiff set > 1 then
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

        Dict _ _ _ left right ->
            height right - height left


balanceSubtrees : Dict comparable v -> Dict comparable v
balanceSubtrees set =
    case set of
        Empty ->
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
