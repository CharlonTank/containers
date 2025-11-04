module MultiSeqDictTests exposing (tests)

import Expect
import MultiSeqDict exposing (MultiSeqDict)
import SeqSet exposing (SeqSet)
import Test exposing (..)


{-| Non-comparable custom type -}
type CustomType
    = Foo
    | Bar
    | Baz


tests : Test
tests =
    describe "MultiSeqDict"
        [ describe "with non-comparable types"
            [ test "can create and insert custom types" <|
                \() ->
                    let
                        dict =
                            MultiSeqDict.empty
                                |> MultiSeqDict.insert Foo 1
                                |> MultiSeqDict.insert Foo 2
                                |> MultiSeqDict.insert Bar 3
                    in
                    Expect.equal (MultiSeqDict.size dict) 3
            , test "get returns all values for key" <|
                \() ->
                    let
                        dict =
                            MultiSeqDict.empty
                                |> MultiSeqDict.insert Foo 1
                                |> MultiSeqDict.insert Foo 2
                                |> MultiSeqDict.insert Bar 3

                        values =
                            MultiSeqDict.get Foo dict
                    in
                    Expect.equal (SeqSet.size values) 2
            ]
        , describe "basic operations"
            [ test "empty dict has size 0" <|
                \() ->
                    Expect.equal (MultiSeqDict.size MultiSeqDict.empty) 0
            , test "singleton creates dict with one element" <|
                \() ->
                    let
                        dict =
                            MultiSeqDict.singleton "key" "value"
                    in
                    Expect.equal (MultiSeqDict.size dict) 1
            , test "insert adds values to existing key" <|
                \() ->
                    let
                        dict =
                            MultiSeqDict.empty
                                |> MultiSeqDict.insert "key" "value1"
                                |> MultiSeqDict.insert "key" "value2"

                        values =
                            MultiSeqDict.get "key" dict
                    in
                    Expect.equal (SeqSet.size values) 2
            , test "remove single value works" <|
                \() ->
                    let
                        dict =
                            MultiSeqDict.empty
                                |> MultiSeqDict.insert "key" "value1"
                                |> MultiSeqDict.insert "key" "value2"
                                |> MultiSeqDict.remove "key" "value1"

                        values =
                            MultiSeqDict.get "key" dict
                    in
                    Expect.equal (SeqSet.size values) 1
            ]
        ]
