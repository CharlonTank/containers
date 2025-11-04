module MultiBiSeqDict exposing
    ( MultiBiSeqDict
    , toDict, fromDict, getReverse, uniqueValues, uniqueValuesCount, toReverseList
    , empty, singleton, insert, update, remove, removeAll
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-| A dictionary mapping unique keys to **multiple** values, which
**maintains a mapping from the values back to keys,** allowing for
modelling **many-to-many relationships.**

Example usage:

    manyToMany : MultiBiSeqDict String Int
    manyToMany =
        MultiBiSeqDict.empty
            |> MultiBiSeqDict.insert "A" 1
            |> MultiBiSeqDict.insert "B" 2
            |> MultiBiSeqDict.insert "C" 3
            |> MultiBiSeqDict.insert "A" 2

    MultiBiSeqDict.get "A" manyToMany
    --> Set.fromList [1, 2]

    MultiBiSeqDict.getReverse 2 manyToMany
    --> Set.fromList ["A", "B"]


# Dictionaries

@docs MultiBiSeqDict


# Differences from Dict

@docs toDict, fromDict, getReverse, uniqueValues, uniqueValuesCount, toReverseList


# Build

@docs empty, singleton, insert, update, remove, removeAll


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge

-}

import SeqDict exposing (SeqDict)

import Set exposing (Set)


{-| The underlying data structure. Think about it as

    type alias MultiBiSeqDict comparable1 comparable2 =
        { forward : SeqDict comparable1 (Set comparable2) -- just a normal Dict!
        , reverse : SeqDict comparable2 (Set comparable1) -- the reverse mappings!
        }

-}
type MultiBiSeqDict comparable1 comparable2
    = MultiBiSeqDict
        { forward : SeqDict comparable1 (Set comparable2)
        , reverse : SeqDict comparable2 (Set comparable1)
        }


{-| Create an empty dictionary.
-}
empty : MultiBiSeqDict comparable1 comparable2
empty =
    MultiBiSeqDict
        { forward = SeqDict.empty
        , reverse = SeqDict.empty
        }


{-| Create a dictionary with one key-value pair.
-}
singleton : comparable1 -> comparable2 -> MultiBiSeqDict comparable1 comparable2
singleton from to =
    MultiBiSeqDict
        { forward = SeqDict.singleton from (Set.singleton to)
        , reverse = SeqDict.singleton to (Set.singleton from)
        }


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : comparable1 -> comparable2 -> MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2
insert from to (MultiBiSeqDict d) =
    SeqDict.update
        from
        (\maybeSet ->
            case maybeSet of
                Nothing ->
                    Just (Set.singleton to)

                Just set ->
                    Just (Set.insert to set)
        )
        d.forward
        |> fromDict


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : comparable1 -> (Set comparable2 -> Set comparable2) -> MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2
update from fn (MultiBiSeqDict d) =
    SeqDict.update from (Maybe.andThen (normalizeSet << fn)) d.forward
        |> fromDict


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
removeAll : comparable1 -> MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2
removeAll from (MultiBiSeqDict d) =
    MultiBiSeqDict
        { d
            | forward = SeqDict.remove from d.forward
            , reverse = SeqDict.filterMap (\_ set -> Set.remove from set |> normalizeSet) d.reverse
        }


{-| Remove a single key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable1 -> comparable2 -> MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2
remove from to (MultiBiSeqDict d) =
    SeqDict.update from (Maybe.andThen (Set.remove to >> normalizeSet)) d.forward
        |> fromDict


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : MultiBiSeqDict comparable1 comparable2 -> Bool
isEmpty (MultiBiSeqDict d) =
    SeqDict.isEmpty d.forward


{-| Determine if a key is in a dictionary.
-}
member : comparable1 -> MultiBiSeqDict comparable1 comparable2 -> Bool
member from (MultiBiSeqDict d) =
    SeqDict.member from d.forward


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : comparable1 -> MultiBiSeqDict comparable1 comparable2 -> Set comparable2
get from (MultiBiSeqDict d) =
    SeqDict.get from d.forward
        |> Maybe.withDefault Set.empty


{-| Get the keys associated with a value. If the value is not found,
return an empty set.
-}
getReverse : comparable2 -> MultiBiSeqDict comparable1 comparable2 -> Set comparable1
getReverse to (MultiBiSeqDict d) =
    SeqDict.get to d.reverse
        |> Maybe.withDefault Set.empty


{-| Determine the number of key-value pairs in the dictionary.
-}
size : MultiBiSeqDict comparable1 comparable2 -> Int
size (MultiBiSeqDict d) =
    SeqDict.foldl (\_ set acc -> Set.size set + acc) 0 d.forward


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ 0, 1 ]

-}
keys : MultiBiSeqDict comparable1 comparable2 -> List comparable1
keys (MultiBiSeqDict d) =
    SeqDict.keys d.forward


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ "Alice", "Bob" ]

-}
values : MultiBiSeqDict comparable1 comparable2 -> List comparable2
values (MultiBiSeqDict d) =
    SeqDict.values d.forward
        |> List.concatMap Set.toList


{-| Get a list of unique values in the dictionary.
-}
uniqueValues : MultiBiSeqDict comparable1 comparable2 -> List comparable2
uniqueValues (MultiBiSeqDict d) =
    SeqDict.keys d.reverse


{-| Get a count of unique values in the dictionary.
-}
uniqueValuesCount : MultiBiSeqDict comparable1 comparable2 -> Int
uniqueValuesCount (MultiBiSeqDict d) =
    SeqDict.size d.reverse


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : MultiBiSeqDict comparable1 comparable2 -> List ( comparable1, Set comparable2 )
toList (MultiBiSeqDict d) =
    SeqDict.toList d.forward


{-| Convert a dictionary into a reverse association list of value-keys pairs.
-}
toReverseList : MultiBiSeqDict comparable1 comparable2 -> List ( comparable2, Set comparable1 )
toReverseList (MultiBiSeqDict d) =
    SeqDict.toList d.reverse


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable1, Set comparable2 ) -> MultiBiSeqDict comparable1 comparable2
fromList list =
    SeqDict.fromList list
        |> fromDict


{-| Apply a function to all values in a dictionary.
-}
map : (comparable1 -> comparable21 -> comparable22) -> MultiBiSeqDict comparable1 comparable21 -> MultiBiSeqDict comparable1 comparable22
map fn (MultiBiSeqDict d) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.map (\key set -> Set.map (fn key) set) d.forward
        |> fromDict


{-| Convert MultiBiSeqDict into a SeqDict. (Throw away the reverse mapping.)
-}
toDict : MultiBiSeqDict comparable1 comparable2 -> SeqDict comparable1 (Set comparable2)
toDict (MultiBiSeqDict d) =
    d.forward


{-| Convert Dict into a MultiBiSeqDict. (Compute the reverse mapping.)
-}
fromDict : SeqDict comparable1 (Set comparable2) -> MultiBiSeqDict comparable1 comparable2
fromDict forward =
    MultiBiSeqDict
        { forward = forward
        , reverse =
            SeqDict.foldl
                (\key set acc ->
                    Set.foldl
                        (\value acc_ ->
                            SeqDict.update
                                value
                                (\maybeSet ->
                                    case maybeSet of
                                        Nothing ->
                                            Just (Set.singleton key)

                                        Just set_ ->
                                            Just (Set.insert key set_)
                                )
                                acc_
                        )
                        acc
                        set
                )
                SeqDict.empty
                forward
        }


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.


    getAges users =
        SeqDict.foldl addAge [] users

    addAge _ user ages =
        user.age :: ages

    -- getAges users == [33,19,28]

-}
foldl : (comparable1 -> Set comparable2 -> acc -> acc) -> acc -> MultiBiSeqDict comparable1 comparable2 -> acc
foldl fn zero (MultiBiSeqDict d) =
    SeqDict.foldl fn zero d.forward


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.


    getAges users =
        SeqDict.foldr addAge [] users

    addAge _ user ages =
        user.age :: ages

    -- getAges users == [28,19,33]

-}
foldr : (comparable1 -> Set comparable2 -> acc -> acc) -> acc -> MultiBiSeqDict comparable1 comparable2 -> acc
foldr fn zero (MultiBiSeqDict d) =
    SeqDict.foldr fn zero d.forward


{-| Keep only the mappings that pass the given test.
-}
filter : (comparable1 -> comparable2 -> Bool) -> MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2
filter fn (MultiBiSeqDict d) =
    SeqDict.toList d.forward
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
partition : (comparable1 -> Set comparable2 -> Bool) -> MultiBiSeqDict comparable1 comparable2 -> ( MultiBiSeqDict comparable1 comparable2, MultiBiSeqDict comparable1 comparable2 )
partition fn (MultiBiSeqDict d) =
    -- TODO diff instead of throwing away and creating from scratch?
    let
        ( forwardTrue, forwardFalse ) =
            SeqDict.partition fn d.forward
    in
    ( fromDict forwardTrue
    , fromDict forwardFalse
    )


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2
union (MultiBiSeqDict left) (MultiBiSeqDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.union left.forward right.forward
        |> fromDict


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2
intersect (MultiBiSeqDict left) (MultiBiSeqDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.intersect left.forward right.forward
        |> fromDict


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2 -> MultiBiSeqDict comparable1 comparable2
diff (MultiBiSeqDict left) (MultiBiSeqDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.diff left.forward right.forward
        |> fromDict


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
    -> MultiBiSeqDict comparable1 comparable21
    -> MultiBiSeqDict comparable1 comparable22
    -> acc
    -> acc
merge fnLeft fnBoth fnRight (MultiBiSeqDict left) (MultiBiSeqDict right) zero =
    SeqDict.merge fnLeft fnBoth fnRight left.forward right.forward zero
