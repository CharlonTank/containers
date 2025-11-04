module BiSeqDictTests exposing (tests)

import BiSeqDict exposing (BiSeqDict)
import Expect
import SeqSet exposing (SeqSet)
import Test exposing (..)


{-| Non-comparable custom type to prove no comparable constraint -}
type CustomType
    = Foo
    | Bar
    | Baz


type alias CustomRecord =
    { name : String
    , value : Int
    }


tests : Test
tests =
    describe "BiSeqDict"
        [ describe "with non-comparable types"
            [ test "can create and insert custom types" <|
                \() ->
                    let
                        dict =
                            BiSeqDict.empty
                                |> BiSeqDict.insert Foo "hello"
                                |> BiSeqDict.insert Bar "world"
                                |> BiSeqDict.insert Baz "hello"
                    in
                    Expect.equal (BiSeqDict.size dict) 3
            , test "getReverse works with custom types" <|
                \() ->
                    let
                        dict =
                            BiSeqDict.empty
                                |> BiSeqDict.insert Foo "hello"
                                |> BiSeqDict.insert Bar "world"
                                |> BiSeqDict.insert Baz "hello"

                        result =
                            BiSeqDict.getReverse "hello" dict
                    in
                    Expect.equal (SeqSet.size result) 2
            , test "can use custom records as values" <|
                \() ->
                    let
                        dict =
                            BiSeqDict.empty
                                |> BiSeqDict.insert "alice" { name = "Alice", value = 1 }
                                |> BiSeqDict.insert "bob" { name = "Bob", value = 2 }
                                |> BiSeqDict.insert "charlie" { name = "Alice", value = 1 }

                        record =
                            { name = "Alice", value = 1 }

                        reverseKeys =
                            BiSeqDict.getReverse record dict
                    in
                    Expect.equal (SeqSet.size reverseKeys) 2
            ]
        , describe "basic operations"
            [ test "empty dict has size 0" <|
                \() ->
                    Expect.equal (BiSeqDict.size BiSeqDict.empty) 0
            , test "singleton creates dict with one element" <|
                \() ->
                    let
                        dict =
                            BiSeqDict.singleton "key" "value"
                    in
                    Expect.equal (BiSeqDict.size dict) 1
            , test "get returns inserted value" <|
                \() ->
                    let
                        dict =
                            BiSeqDict.singleton "key" "value"
                    in
                    Expect.equal (BiSeqDict.get "key" dict) (Just "value")
            , test "remove works" <|
                \() ->
                    let
                        dict =
                            BiSeqDict.singleton "key" "value"
                                |> BiSeqDict.remove "key"
                    in
                    Expect.equal (BiSeqDict.size dict) 0
            ]
        ]
