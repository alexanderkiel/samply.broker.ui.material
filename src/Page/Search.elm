module Page.Search exposing
    ( Config
    , Model
    , init
    , Msg
    , update
    , view
    , subscriptions
    , eventSubscriptions
    )

{-| The search page displays a single search which can be edited.


# Model

@docs Config
@docs Model
@docs init


# Update

@docs Msg
@docs update


# View

@docs view


# Subscriptions

@docs subscriptions
@docs eventSubscriptions

-}

import Browser exposing (Document)
import Data.Command as Command exposing (SyncToken, commandBuilder, jsonCommand)
import Data.Event exposing (Event)
import Data.Interval as Interval
import Data.LoadingStatus as LoadingStatus exposing (LoadingStatus(..))
import Data.Mdr.DataElement as DataElement exposing (DataElement, DataElementDetail)
import Data.Mdr.DataElementGroup exposing (DataElementGroup)
import Data.Name exposing (Name(..))
import Data.Search exposing (Id, Search)
import Data.Search.Criterion as Criterion exposing (Criterion)
import Data.Urn exposing (Urn)
import Dict exposing (Dict)
import EventSub exposing (EventSub)
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import Material.Button as Button
import Material.Card as Card
import Material.Dialog as Dialog
import Material.Fab as Fab
import Material.Icon as Icon
import Material.LayoutGrid as LayoutGrid
import Material.List as List
import Material.Options as Options
import Material.TopAppBar as TopAppBar
import Page.Search.AddGroupCriterionDialog as AddGroupCriterionDialog
import Page.Search.EditCriterionDialog as EditCriterionDialog
import Process
import Request.Command
import Request.Error as Request
import Request.Mdr
import Request.Search
import Task exposing (Task)


{-| The model.

    The model is a state machine. It starts with (NormalLoading Start) and has
    two end states: Loaded and Failed.

-}
type Model
    = NormalLoading LoadingModel
    | SlowLoading LoadingModel
    | Loaded LoadedModel
    | Failed FailedModel


type LoadingModel
    = Start String
    | SearchLoaded String SearchModel
    | SearchLoadingFailed String Request.Error
    | GroupsLoaded String (Dict Urn Group)
    | GroupLoadingFailed String Request.Error


type alias SearchModel =
    { id : Id
    , title : String
    , criteria : Dict String CriterionModel
    }


type alias CriterionModel =
    { mdrKey : Urn
    , query : Criterion.Query
    , loadingCriterionDetail : LoadingStatus CriterionDetail
    }


type alias CriterionDetail =
    { elementDetail : DataElementDetail
    , dialog : EditCriterionDialog.Model
    }


type alias Group =
    { group : DataElementGroup
    , loadingMembers : LoadingStatus (List DataElement)
    , addGroupCriterionDialog : AddGroupCriterionDialog.Model
    }


type alias LoadedModel =
    { mdrRoot : String
    , search : SearchModel
    , groups : Dict Urn Group
    , openDialogRef : Maybe DialogRef
    , dialogActionInProgress : Bool
    }


type DialogRef
    = AddGroupCriterionDialogRef Urn
    | EditCriterionDialogRef Urn


type alias FailedModel =
    { searchError : Request.Error
    , groupsError : Request.Error
    }


initLoadedModel mdrRoot search groups =
    { mdrRoot = mdrRoot
    , search = search
    , groups = groups
    , openDialogRef = Nothing
    , dialogActionInProgress = False
    }


initSearchModel : Search -> SearchModel
initSearchModel { id, title, criteria } =
    { id = id
    , title = title
    , criteria =
        criteria
            |> List.map initCriterionModel
            |> List.map (\({ mdrKey } as model) -> ( mdrKey, model ))
            |> Dict.fromList
    }


initCriterionModel : Criterion -> CriterionModel
initCriterionModel { mdrKey, query } =
    { mdrKey = mdrKey
    , query = query
    , loadingCriterionDetail = LoadingStatus.Loading
    }


initCriterionDetail : DataElementDetail -> Criterion.Query -> CriterionDetail
initCriterionDetail elementDetail query =
    { elementDetail = elementDetail
    , dialog = EditCriterionDialog.init elementDetail <| Just query
    }


type alias Config =
    { mdrRoot : String
    , mdrNamespace : String
    }


{-| Initializes the model with config, an optional sync token and the searches
identifier.

    Tasks are spawn to load the search and metadata. Search loads are
    synchronized if the optional sync token is present.

-}
init : Config -> Maybe SyncToken -> Id -> ( Model, Cmd Msg )
init { mdrRoot, mdrNamespace } searchStoreSyncToken id =
    ( NormalLoading <| Start mdrRoot
    , Cmd.batch
        [ Request.Search.search searchStoreSyncToken id
            |> Task.attempt (SearchResult >> StartMsg)
        , loadNamespaceMembers mdrRoot mdrNamespace
        , Task.perform (\_ -> PassedSlowLoadThreshold) LoadingStatus.slowThreshold
        ]
    )


loadNamespaceMembers : String -> String -> Cmd Msg
loadNamespaceMembers mdrRoot name =
    Request.Mdr.namespaceMembers mdrRoot name
        |> Task.attempt (GroupsResult >> StartMsg)



---- UPDATE -------------------------------------------------------------------


type Msg
    = StartMsg StartMsg
    | LoadedMsg LoadedMsg
    | PassedSlowLoadThreshold
    | DataElementGroupMembersLoaded Urn (Result Request.Error (List DataElement))


type StartMsg
    = SearchResult (Result Request.Error Search)
    | GroupsResult (Result Request.Error (List DataElementGroup))


type LoadedMsg
    = DataElementDetailLoaded Urn (Result Request.Error DataElementDetail)
    | OpenAddCriterionDialog
    | OpenAddGroupCriterionDialog Urn
    | CloseAddGroupCriterionDialog
    | AddGroupCriterionDialogMsg Urn AddGroupCriterionDialog.Msg
    | AddGroupCriterion DataElementDetail Criterion
    | CriterionAdded DataElementDetail Criterion (Result Request.Error Command.Result)
    | OpenCriterionDialog Urn
    | CloseCriterionDialog Urn
    | CriterionDialogMsg Urn EditCriterionDialog.Msg
    | SaveCriterion Criterion
    | CriterionSaved Criterion (Result Request.Error Command.Result)
    | RemoveCriterion Urn
    | CriterionRemoved Urn (Result Request.Error Command.Result)
    | CriterionAddedEvent Event
    | CriterionEditedEvent Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Messages before Loaded or Failed state is reached
        StartMsg startMsg ->
            let
                updateLoadingModel lift loadingTag loadingModel =
                    let
                        ( result, cmd ) =
                            updateAtStart startMsg loadingModel loadingTag
                    in
                    case result of
                        StillLoading newLoadingModel ->
                            ( lift newLoadingModel, cmd )

                        FinishedLoading loadedModel ->
                            ( Loaded loadedModel
                            , loadedModel.search.criteria
                                |> Dict.values
                                |> List.map (.mdrKey >> loadElementDetail loadedModel.mdrRoot)
                                |> List.map (Cmd.map LoadedMsg)
                                |> (::) cmd
                                |> Cmd.batch
                            )
            in
            case model of
                NormalLoading loadingModel ->
                    updateLoadingModel NormalLoading Loading loadingModel

                SlowLoading loadingModel ->
                    updateLoadingModel SlowLoading LoadingSlowly loadingModel

                -- End state
                Loaded _ ->
                    ( model, Cmd.none )

                -- End state
                Failed _ ->
                    ( model, Cmd.none )

        -- Messages after Loaded state is reached
        LoadedMsg loadedMsg ->
            case model of
                Loaded loadedModel ->
                    updateAfterLoading loadedMsg loadedModel
                        |> Tuple.mapBoth Loaded (Cmd.map LoadedMsg)

                -- Ignore loaded messages for other states as Loaded.
                _ ->
                    ( model, Cmd.none )

        PassedSlowLoadThreshold ->
            ( case model of
                NormalLoading loadingModel ->
                    SlowLoading <| markAsSlowLoading loadingModel

                Loaded loadedModel ->
                    Loaded { loadedModel | groups = markGroupsAsSlowLoading loadedModel.groups }

                _ ->
                    model
            , Cmd.none
            )

        DataElementGroupMembersLoaded groupId result ->
            case result of
                Ok members ->
                    let
                        relevantMembers =
                            List.filter isDataElementRelevant members

                        updateLoadingModel lift loadingModel =
                            case loadingModel of
                                GroupsLoaded mdrRoot groups ->
                                    setGroupElements groupId relevantMembers groups
                                        |> GroupsLoaded mdrRoot
                                        |> lift

                                -- Ignore elements here, because we are at least
                                -- in GroupsLoaded state before loading the
                                -- members.
                                _ ->
                                    lift loadingModel
                    in
                    ( case model of
                        NormalLoading loadingModel ->
                            updateLoadingModel NormalLoading loadingModel

                        SlowLoading loadingModel ->
                            updateLoadingModel SlowLoading loadingModel

                        Loaded loadedModel ->
                            Loaded { loadedModel | groups = setGroupElements groupId relevantMembers loadedModel.groups }

                        -- End state
                        Failed _ ->
                            model
                    , Cmd.none
                    )

                Err error ->
                    ( model
                    , Cmd.none
                    )


isDataElementRelevant : DataElement -> Bool
isDataElementRelevant { designation } =
    String.contains "ID" designation |> not


markAsSlowLoading loadingModel =
    case loadingModel of
        SearchLoaded mdrRoot search ->
            SearchLoaded mdrRoot <| markSearchModelAsSlowLoading search

        GroupsLoaded mdrRoot groups ->
            GroupsLoaded mdrRoot <| markGroupsAsSlowLoading groups

        _ ->
            loadingModel


markGroupsAsSlowLoading =
    Dict.map <|
        \_ group ->
            let
                newLoadingMembers =
                    case group.loadingMembers of
                        LoadingStatus.Loading ->
                            LoadingStatus.LoadingSlowly

                        _ ->
                            group.loadingMembers
            in
            { group | loadingMembers = newLoadingMembers }


markSearchModelAsSlowLoading search =
    let
        updateCriteria f =
            { search | criteria = f search.criteria }
    in
    updateCriteria <| Dict.map markLoadingElementDetailAsSlowLoading


markLoadingElementDetailAsSlowLoading _ criterion =
    case criterion.loadingCriterionDetail of
        LoadingStatus.Loading ->
            { criterion | loadingCriterionDetail = LoadingStatus.LoadingSlowly }

        _ ->
            criterion


type UpdateAtStartResult
    = StillLoading LoadingModel
    | FinishedLoading LoadedModel


setGroupElements : Urn -> List DataElement -> Dict Urn Group -> Dict Urn Group
setGroupElements groupId elements =
    (\group -> { group | loadingMembers = LoadingStatus.Loaded elements })
        |> Maybe.map
        |> Dict.update groupId


updateAtStart :
    StartMsg
    -> LoadingModel
    -> LoadingStatus (List DataElement)
    -> ( UpdateAtStartResult, Cmd Msg )
updateAtStart msg model loadingTag =
    case msg of
        SearchResult result ->
            case result of
                Ok search ->
                    ( let
                        searchModel =
                            initSearchModel search
                      in
                      case model of
                        Start mdrRoot ->
                            searchModel
                                |> SearchLoaded mdrRoot
                                |> StillLoading

                        SearchLoaded _ _ ->
                            StillLoading model

                        SearchLoadingFailed mdrRoot _ ->
                            searchModel
                                |> SearchLoaded mdrRoot
                                |> StillLoading

                        GroupsLoaded mdrRoot groups ->
                            initLoadedModel mdrRoot searchModel groups
                                |> FinishedLoading

                        GroupLoadingFailed mdrRoot _ ->
                            searchModel
                                |> SearchLoaded mdrRoot
                                |> StillLoading
                    , Cmd.none
                    )

                Err error ->
                    ( case model of
                        Start mdrRoot ->
                            StillLoading <| SearchLoadingFailed mdrRoot error

                        SearchLoaded _ _ ->
                            StillLoading <| model

                        SearchLoadingFailed mdrRoot _ ->
                            StillLoading <| SearchLoadingFailed mdrRoot error

                        GroupsLoaded mdrRoot groups ->
                            StillLoading <| SearchLoadingFailed mdrRoot error

                        GroupLoadingFailed mdrRoot _ ->
                            StillLoading <| SearchLoadingFailed mdrRoot error
                    , Cmd.none
                    )

        GroupsResult result ->
            case result of
                Ok members ->
                    let
                        relevantGroups =
                            List.filter relevantGroup members

                        groups =
                            relevantGroups
                                |> List.map
                                    (\({ id } as group) ->
                                        ( id
                                        , { group = group
                                          , loadingMembers = loadingTag
                                          , addGroupCriterionDialog = AddGroupCriterionDialog.init (getMdrRoot model) []
                                          }
                                        )
                                    )
                                |> Dict.fromList
                    in
                    ( case model of
                        Start mdrRoot ->
                            StillLoading <| GroupsLoaded mdrRoot groups

                        SearchLoaded mdrRoot search ->
                            FinishedLoading <| initLoadedModel mdrRoot search groups

                        SearchLoadingFailed mdrRoot _ ->
                            StillLoading <| GroupsLoaded mdrRoot groups

                        GroupsLoaded _ _ ->
                            StillLoading <| model

                        GroupLoadingFailed mdrRoot _ ->
                            StillLoading <| GroupsLoaded mdrRoot groups
                    , relevantGroups
                        |> List.map (.id >> (loadDataElementGroupMembers <| getMdrRoot model))
                        |> Cmd.batch
                    )

                Err error ->
                    ( case model of
                        Start mdrRoot ->
                            StillLoading <| GroupLoadingFailed mdrRoot error

                        SearchLoaded mdrRoot _ ->
                            StillLoading <| GroupLoadingFailed mdrRoot error

                        SearchLoadingFailed mdrRoot _ ->
                            StillLoading <| GroupLoadingFailed mdrRoot error

                        GroupsLoaded _ _ ->
                            StillLoading <| model

                        GroupLoadingFailed mdrRoot _ ->
                            StillLoading <| GroupLoadingFailed mdrRoot error
                    , Cmd.none
                    )


getMdrRoot : LoadingModel -> String
getMdrRoot model =
    case model of
        Start mdrRoot ->
            mdrRoot

        SearchLoaded mdrRoot _ ->
            mdrRoot

        SearchLoadingFailed mdrRoot _ ->
            mdrRoot

        GroupsLoaded mdrRoot _ ->
            mdrRoot

        GroupLoadingFailed mdrRoot _ ->
            mdrRoot


{-| Update function which is used after essential data is loaded.
-}
updateAfterLoading : LoadedMsg -> LoadedModel -> ( LoadedModel, Cmd LoadedMsg )
updateAfterLoading msg model =
    case msg of
        DataElementDetailLoaded elementId result ->
            case result of
                Ok element ->
                    ( addElementDetail elementId element model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        OpenAddCriterionDialog ->
            ( model
            , Cmd.none
            )

        OpenAddGroupCriterionDialog groupId ->
            ( (updateGroup groupId (initAddGroupCriterionDialogModel model.mdrRoot model.search)
                >> openDialog (AddGroupCriterionDialogRef groupId)
              )
                model
            , Cmd.none
            )

        CloseAddGroupCriterionDialog ->
            ( closeDialog model, Cmd.none )

        AddGroupCriterionDialogMsg groupId dialogMsg ->
            updateGroup2
                groupId
                (AddGroupCriterionDialog.update dialogMsg
                    |> updateAddGroupCriterionDialogModel
                )
                model
                |> Tuple.mapSecond (Cmd.map (AddGroupCriterionDialogMsg groupId))

        AddGroupCriterion elementDetail criterion ->
            ( setDialogActionInProgress True model
            , addCriterionTask model.search.id criterion
                |> Task.attempt (CriterionAdded elementDetail criterion)
            )

        OpenCriterionDialog mdrKey ->
            ( openDialog (EditCriterionDialogRef mdrKey) model, Cmd.none )

        CloseCriterionDialog elementId ->
            ( closeDialog model, Cmd.none )

        CriterionDialogMsg mdrKey dialogMsg ->
            ( updateCriterion
                mdrKey
                (EditCriterionDialog.update dialogMsg
                    |> updateCriterionDialogModel
                    |> LoadingStatus.mapLoaded
                    |> updateLoadingCriterionDetail
                    |> Maybe.map
                )
                model
            , Cmd.none
            )

        SaveCriterion criterion ->
            ( setDialogActionInProgress True model
            , editCriterionTask model.search.id criterion
                |> Task.attempt (CriterionSaved criterion)
            )

        RemoveCriterion elementId ->
            ( setDialogActionInProgress True model
            , removeCriterionTask model.search.id elementId
                |> Task.attempt (CriterionRemoved elementId)
            )

        CriterionAdded elementDetail criterion result ->
            case result of
                Ok _ ->
                    ( model
                        |> addCriterion criterion
                        |> updateCriterionElementDetail elementDetail
                        |> closeDialog
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        CriterionSaved criterion result ->
            case result of
                Ok _ ->
                    ( (setCriterion criterion >> closeDialog) model
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        CriterionRemoved elementId result ->
            case result of
                Ok _ ->
                    ( (removeCriterion elementId
                        >> setDialogActionInProgress False
                      )
                        model
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        CriterionAddedEvent event ->
            case decodeValue Criterion.decoder event.data of
                Ok criterion ->
                    ( addCriterion criterion model
                    , loadElementDetail model.mdrRoot criterion.mdrKey
                    )

                Err error ->
                    ( model, Cmd.none )

        CriterionEditedEvent event ->
            case decodeValue Criterion.decoder event.data of
                Ok criterion ->
                    ( setCriterion criterion model
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )


addElementDetail : Urn -> DataElementDetail -> LoadedModel -> LoadedModel
addElementDetail elementId elementDetail model =
    let
        criterionDetail query =
            LoadingStatus.Loaded <| initCriterionDetail elementDetail query

        setElementDetail mdrKey ({ query } as criterion) =
            if mdrKey == elementId then
                { criterion | loadingCriterionDetail = criterionDetail query }

            else
                criterion

        insertElementDetail search =
            { search | criteria = Dict.map setElementDetail search.criteria }

        updateSearch f loadedModel =
            { loadedModel | search = f loadedModel.search }
    in
    updateSearch insertElementDetail model


openDialog : DialogRef -> LoadedModel -> LoadedModel
openDialog dialogRef model =
    { model | openDialogRef = Just dialogRef }


closeDialog : LoadedModel -> LoadedModel
closeDialog model =
    { model | openDialogRef = Nothing, dialogActionInProgress = False }


initAddGroupCriterionDialogModel : String -> SearchModel -> Group -> Group
initAddGroupCriterionDialogModel mdrRoot { criteria } group =
    let
        usedMdrKeys =
            Dict.keys criteria

        dialog =
            AddGroupCriterionDialog.init mdrRoot usedMdrKeys
    in
    { group | addGroupCriterionDialog = dialog }


updateAddGroupCriterionDialogModel f group =
    let
        ( newDialog, cmd ) =
            f group.addGroupCriterionDialog
    in
    ( { group | addGroupCriterionDialog = newDialog }, cmd )


setDialogActionInProgress : Bool -> LoadedModel -> LoadedModel
setDialogActionInProgress inProgress model =
    { model | dialogActionInProgress = inProgress }


updateGroup groupId f model =
    let
        updateGroups =
            f
                |> Maybe.map
                |> Dict.update groupId
    in
    { model | groups = updateGroups model.groups }


{-| Updates the group with `groupId` inside `model` and carries a command.
-}
updateGroup2 :
    Urn
    -> (Group -> ( Group, Cmd msg ))
    -> LoadedModel
    -> ( LoadedModel, Cmd msg )
updateGroup2 groupId f model =
    case Dict.get groupId model.groups of
        Just group ->
            let
                ( newGroup, cmd ) =
                    f group
            in
            ( { model | groups = Dict.insert groupId newGroup model.groups }
            , cmd
            )

        Nothing ->
            ( model, Cmd.none )


updateCriterionDialogModel f criterionDetail =
    { criterionDetail | dialog = f criterionDetail.dialog }


updateLoadingCriterionDetail f criterion =
    { criterion | loadingCriterionDetail = f criterion.loadingCriterionDetail }


{-| Updates the criterion with the given `mdrKey` using the function `f` in the
list of criteria of the search.
-}
updateCriterion :
    Urn
    -> (Maybe CriterionModel -> Maybe CriterionModel)
    -> LoadedModel
    -> LoadedModel
updateCriterion mdrKey f model =
    let
        updateSearch search =
            { search | criteria = Dict.update mdrKey f search.criteria }
    in
    { model | search = updateSearch model.search }


addCriterionTask : Id -> Criterion -> Task Request.Error Command.Result
addCriterionTask id criterion =
    jsonCommand (Name "search" "add-criterion")
        (Criterion.encodeWithSearchId id criterion)
        |> Request.Command.perform


editCriterionTask : Id -> Criterion -> Task Request.Error Command.Result
editCriterionTask id criterion =
    jsonCommand (Name "search" "edit-criterion")
        (Criterion.encodeWithSearchId id criterion)
        |> Request.Command.perform


addCriterion : Criterion -> LoadedModel -> LoadedModel
addCriterion criterion =
    let
        criterionModel =
            initCriterionModel criterion
    in
    updateCriterion criterion.mdrKey (\_ -> Just criterionModel)


updateCriterionElementDetail : DataElementDetail -> LoadedModel -> LoadedModel
updateCriterionElementDetail elementDetail =
    let
        criterionDetail query =
            initCriterionDetail elementDetail query
                |> LoadingStatus.Loaded

        updateModel ({ query } as model) =
            { model | loadingCriterionDetail = criterionDetail query }
    in
    updateCriterion elementDetail.id <| Maybe.map updateModel


removeCriterionTask : Id -> Urn -> Task Request.Error Command.Result
removeCriterionTask id elementId =
    commandBuilder (Name "search" "remove-criterion")
        |> Command.addStringParam "search-id" id
        |> Command.addStringParam "data-element-id" elementId
        |> Command.build
        |> Request.Command.perform


setCriterion : Criterion -> LoadedModel -> LoadedModel
setCriterion ({ mdrKey } as criterion) =
    updateCriterion mdrKey <|
        Maybe.map <|
            \model -> { model | query = criterion.query }


removeCriterion : Urn -> LoadedModel -> LoadedModel
removeCriterion mdrKey =
    updateCriterion mdrKey (\_ -> Nothing)


groupBlacklist : List Urn
groupBlacklist =
    [ "urn:mdr16:dataelementgroup:1:1"
    , "urn:mdr16:dataelementgroup:2:1"
    , "urn:mdr16:dataelementgroup:4:1"
    ]


relevantGroup : DataElementGroup -> Bool
relevantGroup { id } =
    List.member id groupBlacklist |> not


loadDataElementGroupMembers : String -> Urn -> Cmd Msg
loadDataElementGroupMembers mdrRoot groupId =
    Request.Mdr.dataElementGroupMembers mdrRoot groupId
        |> Task.attempt (DataElementGroupMembersLoaded groupId)


loadElementDetail : String -> Urn -> Cmd LoadedMsg
loadElementDetail mdrRoot elementId =
    Request.Mdr.dataElement mdrRoot elementId
        |> Task.attempt (DataElementDetailLoaded elementId)



---- VIEW ---------------------------------------------------------------------


view : Model -> Document Msg
view model =
    let
        title =
            case model of
                Loaded { search } ->
                    search.title

                _ ->
                    ""
    in
    { title = title
    , body =
        appBar title
            :: addGroupCriterionDialogs model
            ++ criterionDialogs model
            ++ body model
    }


appBar : String -> Html msg
appBar name =
    TopAppBar.view []
        [ TopAppBar.section [ TopAppBar.alignStart ]
            [ TopAppBar.navigationIcon "menu"
            , TopAppBar.title name
            ]
        ]


addGroupCriterionDialogs : Model -> List (Html Msg)
addGroupCriterionDialogs model =
    let
        viewDialog openDialogRef dialogActionInProgress { group, loadingMembers, addGroupCriterionDialog } =
            AddGroupCriterionDialog.view
                { onOk = AddGroupCriterion
                , onCancel = CloseAddGroupCriterionDialog
                , map = AddGroupCriterionDialogMsg group.id
                }
                addGroupCriterionDialog
                group
                loadingMembers
                (Just (AddGroupCriterionDialogRef group.id) == openDialogRef)
                dialogActionInProgress
    in
    case model of
        Loaded { groups, openDialogRef, dialogActionInProgress } ->
            groups
                |> Dict.values
                |> List.map (viewDialog openDialogRef dialogActionInProgress)
                |> List.map (Html.map LoadedMsg)

        _ ->
            []


criterionDialogs : Model -> List (Html Msg)
criterionDialogs model =
    let
        renderDialog openDialogRef dialogActionInProgress { mdrKey, elementDetail } =
            EditCriterionDialog.view
                { onOk = SaveCriterion
                , onCancel = CloseCriterionDialog mdrKey
                , map = CriterionDialogMsg mdrKey
                }
                elementDetail.dialog
                (Just (EditCriterionDialogRef mdrKey) == openDialogRef)
                dialogActionInProgress
    in
    case model of
        Loaded { search, openDialogRef, dialogActionInProgress } ->
            search.criteria
                |> Dict.values
                |> List.filterMap maybeLoadedElementDetail
                |> List.map (renderDialog openDialogRef dialogActionInProgress)
                |> List.map (Html.map LoadedMsg)

        _ ->
            []


maybeLoadedElementDetail { mdrKey, loadingCriterionDetail } =
    case loadingCriterionDetail of
        LoadingStatus.Loaded elementDetail ->
            Just { mdrKey = mdrKey, elementDetail = elementDetail }

        _ ->
            Nothing


body : Model -> List (Html Msg)
body model =
    case model of
        NormalLoading _ ->
            []

        SlowLoading _ ->
            [ slowPanel ]

        Loaded loadedModel ->
            loadedBody loadedModel |> List.map (Html.map LoadedMsg)

        Failed { searchError } ->
            case searchError of
                Request.NotFound ->
                    [ notFoundPanel ]

                _ ->
                    [ errorPanel ]


loadedBody : LoadedModel -> List (Html LoadedMsg)
loadedBody { search, groups } =
    [ Html.div [ Attr.class "mdc-top-app-bar--fixed-adjust" ]
        [ criterionList (Dict.values search.criteria) groups ]
    ]


{-| Returns a grouped list of criteria.

    The group designations are used for group titles.

-}
criterionList : List CriterionModel -> Dict Urn Group -> Html LoadedMsg
criterionList criteria groups =
    groups
        |> Dict.values
        |> List.map (renderGroup criteria)
        |> LayoutGrid.view []


{-| Renders a data element group like Donor or Sample.

    Returns a list with the groups designation as subheader and matching
    criteria as items when the group members are loaded. Group members are
    needed to find matching criteria.

    See `criterionItem` for the item rendering.

    Returns an empty criteria list when the members are loading slowly.

    Includes an "add criterion" button at the end of the list.

-}
renderGroup : List CriterionModel -> Group -> Html LoadedMsg
renderGroup criteria { group, loadingMembers } =
    let
        render criterion =
            LayoutGrid.cell []
                [ Card.view []
                    [ Html.h2 [ Attr.class "criteria-group__title" ]
                        [ Html.text group.designation ]
                    , List.view [ List.twoLine ] <|
                        List.map criterionItem criterion
                    , Card.actions []
                        [ addGroupCriterionButton group.id
                        ]
                    ]
                ]

        joinWithMember members ({ mdrKey } as criterion) =
            members
                |> List.filter (\{ id } -> id == mdrKey)
                |> List.head
                |> Maybe.map (\member -> ( criterion, member ))
    in
    case loadingMembers of
        -- Don't show anything on still fast loading members
        Loading ->
            Html.text ""

        -- Members are slow loading, just show what we have
        LoadingSlowly ->
            render []

        -- We have everything, just render the matching criteria
        LoadingStatus.Loaded members ->
            criteria
                |> List.filterMap (joinWithMember members)
                |> render

        -- TODO: show error message
        LoadingStatus.Failed _ ->
            render []


criterionItem : ( CriterionModel, DataElement ) -> ( String, Html LoadedMsg )
criterionItem ( { mdrKey, query, loadingCriterionDetail }, { designation } ) =
    let
        unit =
            case loadingCriterionDetail of
                LoadingStatus.Loaded { elementDetail } ->
                    DataElement.getUnit elementDetail

                _ ->
                    Nothing

        appendUnit str =
            Maybe.map ((++) (str ++ " ")) unit
                |> Maybe.withDefault str

        printFloat =
            String.fromFloat >> appendUnit

        queryText =
            case query of
                Criterion.Enumerated selectedValues ->
                    case loadingCriterionDetail of
                        LoadingStatus.Loading ->
                            ""

                        LoadingStatus.Loaded { elementDetail } ->
                            case elementDetail.validation of
                                DataElement.Enumerated values ->
                                    values
                                        |> List.filter (\{ value } -> List.member value selectedValues)
                                        |> List.map .designation
                                        |> String.join ", "

                                _ ->
                                    String.join ", " selectedValues

                        _ ->
                            String.join ", " selectedValues

                Criterion.Date _ _ ->
                    "Date"

                Criterion.Float metricQuery ->
                    case metricQuery of
                        Criterion.LessThan value ->
                            "x < " ++ printFloat value

                        Criterion.GreaterThan value ->
                            "x > " ++ printFloat value

                        Criterion.Between lowerValue upperValue ->
                            printFloat lowerValue
                                ++ " < x < "
                                ++ printFloat upperValue

                        Criterion.Outside lowerValue upperValue ->
                            "x < "
                                ++ printFloat lowerValue
                                ++ ", "
                                ++ "x > "
                                ++ printFloat upperValue
    in
    ( mdrKey
    , List.item
        [ Options.onClick <| OpenCriterionDialog mdrKey ]
        [ List.text []
            [ List.primaryText [] [ Html.text designation ]
            , List.secondaryText [] [ Html.text queryText ]
            ]
        , Icon.view [ List.meta ] "delete"
        ]
    )


addGroupCriterionButton : Urn -> Html LoadedMsg
addGroupCriterionButton groupId =
    Button.view
        [ Card.actionButton
        , Options.onClick (OpenAddGroupCriterionDialog groupId)
        ]
        [ Html.text "add criterion" ]


notFoundPanel : Html msg
notFoundPanel =
    Html.div [ Attr.class "mdc-top-app-bar--prominent-fixed-adjust" ]
        [ Html.p [ Attr.class "not-found" ] [ Html.text "404" ]
        , Html.p [ Attr.class "text-center" ] [ Html.a [ Attr.href "/" ] [ Html.text "Go Home" ] ]
        ]


slowPanel : Html msg
slowPanel =
    Html.div [ Attr.class "mdc-top-app-bar--prominent-fixed-adjust" ]
        [ Html.p [ Attr.class "slow" ] [ Html.text "Loading..." ]
        ]


errorPanel : Html msg
errorPanel =
    Html.div [ Attr.class "mdc-top-app-bar--prominent-fixed-adjust" ]
        [ Html.p [ Attr.class "text-center" ] [ Html.a [ Attr.href "/" ] [ Html.text "Go Home" ] ]
        ]



---- SUBSCRIPTIONS ------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


eventSubscriptions : Model -> EventSub Msg
eventSubscriptions model =
    case model of
        Loaded { search } ->
            EventSub.batch
                [ EventSub.new
                    [ "search", "criterion-added", search.id ]
                    (CriterionAddedEvent >> LoadedMsg)
                , EventSub.new
                    [ "search", "criterion-edited", search.id ]
                    (CriterionEditedEvent >> LoadedMsg)
                ]

        _ ->
            EventSub.none
