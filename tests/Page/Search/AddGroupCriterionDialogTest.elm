module Page.Search.AddGroupCriterionDialogTest exposing (initTest)

import Expect
import Fuzz exposing (Fuzzer)
import List.Selection as Selection
import Page.Search.AddGroupCriterionDialog exposing (..)
import Test exposing (..)


initTest : Test
initTest =
    describe "Initializing the dialog"
        [ describe "selects no member"
            [ test "on empty member list" <|
                \_ ->
                    let
                        model =
                            init "" [] []
                    in
                    Expect.equal Nothing (selectedMember model)
            , fuzz Fuzz.string "on only used member" <|
                \id ->
                    let
                        model =
                            init ""
                                [ id ]
                                [ { id = id, designation = "" } ]
                    in
                    Expect.equal Nothing (selectedMember model)
            ]
        , describe "selects the only member"
            [ fuzz Fuzz.string "on one unused member" <|
                \id ->
                    let
                        member =
                            { id = id, designation = "" }

                        model =
                            init ""
                                []
                                [ member ]
                    in
                    Expect.equal (Just ( member, True )) (selectedMember model)
            ]
        , describe "selects the first member"
            [ fuzz2 Fuzz.string Fuzz.string "on two unused members" <|
                \id1 id2 ->
                    let
                        member1 =
                            { id = id1, designation = "" }

                        member2 =
                            { id = id2, designation = "" }

                        model =
                            init ""
                                []
                                [ member1, member2 ]
                    in
                    Expect.equal (Just ( member1, True )) (selectedMember model)
            ]
        , describe "selects the second member"
            [ fuzz2 Fuzz.string Fuzz.string "on two members were the first is used" <|
                \id1 id2 ->
                    let
                        member1 =
                            { id = id1, designation = "" }

                        member2 =
                            { id = id2, designation = "" }

                        model =
                            init ""
                                [ id1 ]
                                [ member1, member2 ]
                    in
                    Expect.equal (Just ( member2, True )) (selectedMember model)
            ]
        ]


selectedMember model =
    case model of
        SelectItem selectedItemModel ->
            Selection.selected selectedItemModel.selection

        _ ->
            Nothing
