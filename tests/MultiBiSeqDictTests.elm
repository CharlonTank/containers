module MultiBiSeqDictTests exposing (tests)

import Expect
import MultiBiSeqDict exposing (MultiBiSeqDict)
import SeqSet exposing (SeqSet)
import Test exposing (..)


{-| Non-comparable custom type -}
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
    describe "MultiBiSeqDict"
        [ describe "with non-comparable types"
            [ test "can create and insert custom types" <|
                \() ->
                    let
                        dict =
                            MultiBiSeqDict.empty
                                |> MultiBiSeqDict.insert Foo 1
                                |> MultiBiSeqDict.insert Foo 2
                                |> MultiBiSeqDict.insert Bar 2
                    in
                    Expect.equal (MultiBiSeqDict.size dict) 3
            , test "get returns all values for key" <|
                \() ->
                    let
                        dict =
                            MultiBiSeqDict.empty
                                |> MultiBiSeqDict.insert Foo 1
                                |> MultiBiSeqDict.insert Foo 2
                                |> MultiBiSeqDict.insert Bar 3

                        values =
                            MultiBiSeqDict.get Foo dict
                    in
                    Expect.equal (SeqSet.size values) 2
            , test "getReverse returns all keys for value" <|
                \() ->
                    let
                        dict =
                            MultiBiSeqDict.empty
                                |> MultiBiSeqDict.insert Foo 1
                                |> MultiBiSeqDict.insert Foo 2
                                |> MultiBiSeqDict.insert Bar 2

                        keys =
                            MultiBiSeqDict.getReverse 2 dict
                    in
                    Expect.equal (SeqSet.size keys) 2
            , test "works with custom records" <|
                \() ->
                    let
                        dict =
                            MultiBiSeqDict.empty
                                |> MultiBiSeqDict.insert Foo { name = "alice", value = 1 }
                                |> MultiBiSeqDict.insert Bar { name = "bob", value = 2 }
                                |> MultiBiSeqDict.insert Foo { name = "bob", value = 2 }

                        record =
                            { name = "bob", value = 2 }

                        keys =
                            MultiBiSeqDict.getReverse record dict
                    in
                    Expect.equal (SeqSet.size keys) 2
            ]
        , describe "basic operations"
            [ test "empty dict has size 0" <|
                \() ->
                    Expect.equal (MultiBiSeqDict.size MultiBiSeqDict.empty) 0
            , test "singleton creates dict with one element" <|
                \() ->
                    let
                        dict =
                            MultiBiSeqDict.singleton "key" "value"
                    in
                    Expect.equal (MultiBiSeqDict.size dict) 1
            ]
        ]
