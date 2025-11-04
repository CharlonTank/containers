module MultiSeqDict exposing
    ( MultiSeqDict
    , toDict, fromDict
    , empty, singleton, insert, update, remove, removeAll
    , isEmpty, member, get, size
    , keys, values, toList, fromList, fromFlatList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-| A dictionary mapping unique keys to **multiple** values, allowing for
modelling **one-to-many relationships.**

Example usage:

    oneToMany : MultiSeqDict String Int
    oneToMany =
        MultiSeqDict.empty
            |> MultiSeqDict.insert "A" 1
            |> MultiSeqDict.insert "B" 2
            |> MultiSeqDict.insert "C" 3
            |> MultiSeqDict.insert "A" 2

    MultiSeqDict.get "A" oneToMany
    --> Set.fromList [1, 2]


# Dictionaries

@docs MultiSeqDict


# Differences from Dict

@docs toDict, fromDict


# Build

@docs empty, singleton, insert, update, remove, removeAll


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList, fromFlatList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge

-}

import SeqDict exposing (SeqDict)

import List.Extra
import Set exposing (Set)


{-| The underlying data structure. Think about it as

     type alias MultiSeqDict comparable1 comparable2 =
         Dict comparable1 (Set comparable2) -- just a normal Dict!

-}
type MultiSeqDict comparable1 comparable2
    = MultiSeqDict (SeqDict comparable1 (Set comparable2))


{-| Create an empty dictionary.
-}
empty : MultiSeqDict comparable1 comparable2
empty =
    MultiSeqDict SeqDict.empty


{-| Create a dictionary with one key-value pair.
-}
singleton : comparable1 -> comparable2 -> MultiSeqDict comparable1 comparable2
singleton from to =
    MultiSeqDict (SeqDict.singleton from (Set.singleton to))


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : comparable1 -> comparable2 -> MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2
insert from to (MultiSeqDict d) =
    MultiSeqDict <|
        SeqDict.update
            from
            (\maybeSet ->
                case maybeSet of
                    Nothing ->
                        Just (Set.singleton to)

                    Just set ->
                        Just (Set.insert to set)
            )
            d


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : comparable1 -> (Set comparable2 -> Set comparable2) -> MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2
update from fn (MultiSeqDict d) =
    MultiSeqDict <| SeqDict.update from (Maybe.andThen (normalizeSet << fn)) d


{-| In our model, (Just Set.empty) has the same meaning as Nothing.
Make it be Nothing!
-}
normalizeSet : Set comparable1 -> Maybe (Set comparable1)
normalizeSet set =
    if Set.isEmpty set then
        Nothing

    else
        Just set


{-| Remove all key-value pairs for the given key from a dictionary. If the key is
not found, no changes are made.
-}
removeAll : comparable1 -> MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2
removeAll from (MultiSeqDict d) =
    MultiSeqDict (SeqDict.remove from d)


{-| Remove a single key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable1 -> comparable2 -> MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2
remove from to (MultiSeqDict d) =
    MultiSeqDict <|
        SeqDict.update from (Maybe.andThen (Set.remove to >> normalizeSet)) d


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : MultiSeqDict comparable1 comparable2 -> Bool
isEmpty (MultiSeqDict d) =
    SeqDict.isEmpty d


{-| Determine if a key is in a dictionary.
-}
member : comparable1 -> MultiSeqDict comparable1 comparable2 -> Bool
member from (MultiSeqDict d) =
    SeqDict.member from d


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", "cat"), ("Jerry", "mouse") ]

    get "Tom"   animals == Set.singleton "cat"
    get "Jerry" animals == Set.singleton "mouse"
    get "Spike" animals == Set.empty

-}
get : comparable1 -> MultiSeqDict comparable1 comparable2 -> Set comparable2
get from (MultiSeqDict d) =
    SeqDict.get from d
        |> Maybe.withDefault Set.empty


{-| Determine the number of key-value pairs in the dictionary.
-}
size : MultiSeqDict comparable1 comparable2 -> Int
size (MultiSeqDict d) =
    SeqDict.foldl (\_ set acc -> Set.size set + acc) 0 d


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ 0, 1 ]

-}
keys : MultiSeqDict comparable1 comparable2 -> List comparable1
keys (MultiSeqDict d) =
    SeqDict.keys d


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ "Alice", "Bob" ]

-}
values : MultiSeqDict comparable1 comparable2 -> List comparable2
values (MultiSeqDict d) =
    SeqDict.values d
        |> List.concatMap Set.toList


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : MultiSeqDict comparable1 comparable2 -> List ( comparable1, Set comparable2 )
toList (MultiSeqDict d) =
    SeqDict.toList d


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable1, Set comparable2 ) -> MultiSeqDict comparable1 comparable2
fromList list =
    SeqDict.fromList list
        |> fromDict


{-| Convert an association list into a dictionary.

    fromFlatList
        [ ( "foo", 1 )
        , ( "bar", 2 )
        , ( "foo", 3 )
        ]

results in the same dict as

    fromList
        [ ( "foo", Set.fromList [ 1, 3 ] )
        , ( "bar", Set.fromList [ 2 ] )
        ]

-}
fromFlatList : List ( comparable1, comparable2 ) -> MultiSeqDict comparable1 comparable2
fromFlatList list =
    list
        |> List.Extra.gatherEqualsBy Tuple.first
        |> List.map
            (\( ( key, _ ) as x, xs ) ->
                ( key
                , Set.fromList <| List.map Tuple.second <| x :: xs
                )
            )
        |> SeqDict.fromList
        |> fromDict


{-| Apply a function to all values in a dictionary.
-}
map : (comparable1 -> comparable21 -> comparable22) -> MultiSeqDict comparable1 comparable21 -> MultiSeqDict comparable1 comparable22
map fn (MultiSeqDict d) =
    MultiSeqDict <| SeqDict.map (\key set -> Set.map (fn key) set) d


{-| Convert MultiSeqDict into a SeqDict. (Throw away the reverse mapping.)
-}
toDict : MultiSeqDict comparable1 comparable2 -> SeqDict comparable1 (Set comparable2)
toDict (MultiSeqDict d) =
    d


{-| Convert Dict into a MultiSeqDict. (Compute the reverse mapping.)
-}
fromDict : SeqDict comparable1 (Set comparable2) -> MultiSeqDict comparable1 comparable2
fromDict dict =
    MultiSeqDict dict


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.


    getAges users =
        SeqDict.foldl addAge [] users

    addAge _ user ages =
        user.age :: ages

    -- getAges users == [33,19,28]

-}
foldl : (comparable1 -> Set comparable2 -> acc -> acc) -> acc -> MultiSeqDict comparable1 comparable2 -> acc
foldl fn zero (MultiSeqDict d) =
    SeqDict.foldl fn zero d


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.


    getAges users =
        SeqDict.foldr addAge [] users

    addAge _ user ages =
        user.age :: ages

    -- getAges users == [28,19,33]

-}
foldr : (comparable1 -> Set comparable2 -> acc -> acc) -> acc -> MultiSeqDict comparable1 comparable2 -> acc
foldr fn zero (MultiSeqDict d) =
    SeqDict.foldr fn zero d


{-| Keep only the mappings that pass the given test.
-}
filter : (comparable1 -> comparable2 -> Bool) -> MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2
filter fn (MultiSeqDict d) =
    SeqDict.toList d
        |> List.filterMap
            (\( key, values_ ) ->
                values_
                    |> Set.filter (fn key)
                    |> normalizeSet
                    |> Maybe.map (Tuple.pair key)
            )
        |> fromList


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (comparable1 -> Set comparable2 -> Bool) -> MultiSeqDict comparable1 comparable2 -> ( MultiSeqDict comparable1 comparable2, MultiSeqDict comparable1 comparable2 )
partition fn (MultiSeqDict d) =
    let
        ( true, false ) =
            SeqDict.partition fn d
    in
    ( MultiSeqDict true
    , MultiSeqDict false
    )


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2
union (MultiSeqDict left) (MultiSeqDict right) =
    MultiSeqDict <| SeqDict.union left right


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2
intersect (MultiSeqDict left) (MultiSeqDict right) =
    MultiSeqDict <| SeqDict.intersect left right


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2 -> MultiSeqDict comparable1 comparable2
diff (MultiSeqDict left) (MultiSeqDict right) =
    MultiSeqDict <| SeqDict.diff left right


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

-}
merge :
    (comparable1 -> Set comparable21 -> acc -> acc)
    -> (comparable1 -> Set comparable21 -> Set comparable22 -> acc -> acc)
    -> (comparable1 -> Set comparable22 -> acc -> acc)
    -> MultiSeqDict comparable1 comparable21
    -> MultiSeqDict comparable1 comparable22
    -> acc
    -> acc
merge fnLeft fnBoth fnRight (MultiSeqDict left) (MultiSeqDict right) zero =
    SeqDict.merge fnLeft fnBoth fnRight left right zero
