module Tasks exposing (..)

import Set exposing (Set)
import Array exposing (Array)
import Dict exposing (Dict)
import Element exposing (Element, Attribute, pointer, paddingEach, text, rgb, rgba, el, none, column, row, padding)
import Element.Input exposing (button)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Element.Background as Background
import Icon
import Style
import Utils exposing (ex, unreachable, arraySingleton, todo, removeFromArray, mapArrayAt)


--- Model ---


{-| A unique key for a task
-}
type alias TaskId =
    String


type alias Tasklist =
    Array Task


{-| A path to a task
-}
type alias Path =
    List Int


{-| There are two types of task, one that has subtasks and another that does not.
-}
type Task
    = BranchTask TaskBranch
    | LeafTask TaskLeaf


{-| A task with subtasks
-}
type alias TaskBranch =
    { name : String
    , expanded : Bool
    , subtasks : Tasklist
    }


{-| A task without subtasks
-}
type alias TaskLeaf =
    { name : String
    , completed : Bool
    }


mapTask : (TaskBranch -> a) -> (TaskLeaf -> a) -> Task -> a
mapTask branchMapper leafMapper task =
    case task of
        BranchTask task_ ->
            branchMapper task_

        LeafTask task_ ->
            leafMapper task_


mapBranch : (TaskBranch -> a) -> Task -> Maybe a
mapBranch mapper task =
    case task of
        BranchTask task_ ->
            Just <| mapper task_

        LeafTask _ ->
            Nothing


mapLeaf : (TaskLeaf -> a) -> Task -> Maybe a
mapLeaf mapper task =
    case task of
        LeafTask task_ ->
            Just <| mapper task_

        BranchTask _ ->
            Nothing


{-| Convert a leaf task to a branch task
-}
leafToBranch : TaskLeaf -> TaskBranch
leafToBranch task =
    { expanded = False, name = task.name, subtasks = Array.empty }


taskExpanded : Task -> Bool
taskExpanded =
    mapTask
        (\t -> t.expanded)
        (\_ -> False)


{-| Get the name of a task
-}
name : Task -> String
name task =
    case task of
        BranchTask task_ ->
            task_.name

        LeafTask task_ ->
            task_.name


setName : String -> Task -> Task
setName newName task =
    case task of
        BranchTask task_ ->
            BranchTask { task_ | name = newName }

        LeafTask task_ ->
            LeafTask { task_ | name = newName }


{-| When editing a task, is it a newTask task to be added, or an existing one
-}
type EditType
    = Add
    | Edit


{-| The task that is being edited, and whether it will be a newTask task, or an edit to an existing
task
-}
type alias EditingTask =
    { editType : EditType
    , path : Path
    , text : String
    }


{-| Helper to work out if we are currently adding a task to this tasklist, and if so give the
proposed name of the new task.
-}
addingToThisTasklist : Maybe EditingTask -> Maybe String
addingToThisTasklist =
    Maybe.andThen
        (\editingTask ->
            if editingTask.editType == Add && List.isEmpty editingTask.path then
                Just editingTask.text
            else
                Nothing
        )


editingThisTask : Maybe EditingTask -> Maybe String
editingThisTask =
    Maybe.andThen
        (\editingTask ->
            if editingTask.editType == Edit && List.isEmpty editingTask.path then
                Just editingTask.text
            else
                Nothing
        )


updateEditText : String -> EditingTask -> EditingTask
updateEditText newText editingTask =
    { editingTask | text = newText }


{-| Has this task been completed
-}
completed : Task -> Bool
completed branch =
    Debug.todo "work out if all subtasks have been completed"


newTask : String -> Task
newTask taskName =
    LeafTask
        { name = taskName
        , completed = False
        }


{-| Add a subtask to a task
-}
addSubtask : Task -> Task -> Task
addSubtask subtask =
    mapTask
        (\task -> BranchTask { task | subtasks = Array.push subtask task.subtasks })
        (\task -> BranchTask <| TaskBranch task.name False <| arraySingleton subtask)


{-| Dummy data while building app
-}
dummyTasks : Tasklist
dummyTasks =
    Array.fromList
        [ newTask "Do reading for lit review"
            |> addSubtask (newTask "Read about lymphoma")
            |> addSubtask
                (newTask "Read about user centred design"
                    |> addSubtask (newTask "Read the paper \"10.1001/example.12612\"")
                )
        , newTask "Complete mid-term assignment for epidemiology course"
        , newTask "Tidy room"
        ]



--- Lens-style functions


{-| Updates the task at the given path
-}
updateTaskInTasklist : (Task -> Task) -> Path -> Tasklist -> Tasklist
updateTaskInTasklist updateFn path tasklist =
    case path of
        idx :: pathRest ->
            case Array.get idx tasklist of
                Just task ->
                    Array.set idx (updateTaskInTask updateFn pathRest task) tasklist

                Nothing ->
                    unreachable tasklist

        [] ->
            unreachable tasklist


updateTaskInTask : (Task -> Task) -> Path -> Task -> Task
updateTaskInTask updateFn path task =
    case path of
        [] ->
            updateFn task

        path_ ->
            case task of
                BranchTask task_ ->
                    BranchTask { task_ | subtasks = updateTaskInTasklist updateFn path task_.subtasks }

                LeafTask _ ->
                    unreachable task


{-| Add a task to a tasklist

We have to special-case the encasing tasklist, because we normally add to a tasklist from the
parent task, but the initial tasklist doesn't have a parent.

-}
addTask : String -> Path -> Tasklist -> Tasklist
addTask newTaskName path =
    let
        addTaskToTask =
            mapTask
                (\task ->
                    { task | subtasks = Array.push (newTask newTaskName) task.subtasks }
                )
                (\task ->
                    { name = task.name
                    , expanded = False
                    , subtasks =
                        Array.empty
                    }
                )
                >> BranchTask
    in
        if List.isEmpty path then
            Array.push <| newTask newTaskName
        else
            updateTaskInTasklist addTaskToTask path


updateTask : String -> Path -> Tasklist -> Tasklist
updateTask newTaskName =
    updateTaskInTasklist (setName newTaskName)


{-| Toggle the expansion of the task at the given path
-}
toggleExpand : Path -> Tasklist -> Tasklist
toggleExpand =
    updateTaskInTasklist
        (mapTask
            (\task -> { task | expanded = not task.expanded })
            (unreachable << leafToBranch)
            >> BranchTask
        )


removeTask : Path -> Tasklist -> Tasklist
removeTask path tasklist =
    case path of
        [] ->
            unreachable tasklist

        x :: xs ->
            if List.isEmpty xs then
                removeFromArray x tasklist
            else
                mapArrayAt
                    (mapTask
                        (\task -> { task | subtasks = removeTask xs task.subtasks })
                        (unreachable << leafToBranch)
                        >> BranchTask
                    )
                    x
                    tasklist



--- Update ---


type Msg
    = ToggleExpandTask Path
    | DeleteTask Path
    | UpdateEditText String
    | StartAddTask Path
    | StartEditTask String Path
    | CancelEditTask
    | CommitEditTask
    | TaskToLeaf Path
    | TaskToBranch Path



--- View ---


{-| View function for a list of tasks
-}
viewTasklist : List (Attribute Msg) -> Maybe EditingTask -> Path -> Tasklist -> Element Msg
viewTasklist attrs editingTask path tasklist =
    let
        editingTaskForTask index =
            case editingTask of
                Just editingTask_ ->
                    case editingTask_.path of
                        [] ->
                            Nothing

                        x :: xs ->
                            if index == x then
                                Just { editingTask_ | path = xs }
                            else
                                Nothing

                Nothing ->
                    Nothing

        mapper index task =
            viewTask [] (editingTaskForTask index) (path ++ [ index ]) task

        {-
           if String.isEmpty tasklist.newTaskTaskText then
               Nothing
           else
               Just <| AddTask path tasklist.newTaskTaskText
        -}
        addTaskView =
            case addingToThisTasklist editingTask of
                Just text ->
                    viewTaskEditor [ fill ]
                        { onCommit = CommitEditTask
                        , onCancel = Just CancelEditTask
                        , onChange = UpdateEditText
                        , text = text
                        }

                Nothing ->
                    Style.button
                        []
                        { onPress = Just <| StartAddTask path
                        , label = "Add task"
                        , icon = Just Icon.Add
                        , decoration = Style.TextButton
                        }
    in
        column (indent 20 :: attrs)
            (Array.indexedMap mapper tasklist
                |> Array.push addTaskView
                |> Array.toList
            )


{-| View function for a specific task
-}
viewTask : List (Attribute Msg) -> Maybe EditingTask -> Path -> Task -> Element Msg
viewTask attrs editingTask path task =
    let
        viewSubtasks attrs_ =
            case task of
                BranchTask task_ ->
                    if task_.expanded then
                        viewTasklist [ fill ] editingTask path task_.subtasks
                    else
                        none

                LeafTask task_ ->
                    none

        branchAttrs =
            if taskExpanded task then
                [ Border.width 1
                , Border.color <| rgb 0.8 0.8 0.8
                , Border.rounded 5
                , Background.color <| rgba 0.9 0.9 0.9 0.5
                ]
            else
                []

        textView =
            case editingThisTask editingTask of
                Just text ->
                    viewTaskEditor [ fill ]
                        { onCommit = CommitEditTask
                        , onCancel = Just <| CancelEditTask
                        , onChange = UpdateEditText
                        , text = text
                        }

                Nothing ->
                    viewTaskText
                        [ fill ]
                        { onEdit = StartEditTask (name task) path
                        , onDelete = DeleteTask path
                        , expanded =
                            mapTask
                                (\t -> Just t.expanded)
                                (\_ -> Nothing)
                                task
                        , onToggleExpand = ToggleExpandTask path
                        , text = name task
                        }
    in
        column (fill :: branchAttrs)
            [ Element.el [ fill, taskPadding ] textView
            , viewSubtasks [ Element.alignRight ]
            ]


viewTaskText :
    List (Attribute msg)
    ->
        { onEdit : msg
        , onDelete : msg
        , onToggleExpand : msg
        , expanded : Maybe Bool
        , text : String
        }
    -> Element msg
viewTaskText attrs { onEdit, onDelete, onToggleExpand, expanded, text } =
    Element.row
        attrs
        [ Element.text text
        , Icon.button
            [ Element.alignRight, taskPadding ]
            Icon.Edit
            (Just onEdit)
        , Icon.button
            [ Element.alignRight, taskPadding ]
            Icon.Cancel
            (Just onDelete)
        , case expanded of
            Just expanded_ ->
                let
                    icon_ =
                        if expanded_ then
                            Icon.ExpandMore
                        else
                            Icon.ExpandLess
                in
                    Icon.button
                        [ taskPadding, Element.alignRight ]
                        icon_
                        (Just onToggleExpand)

            Nothing ->
                Icon.blank [ taskPadding ]
        ]


{-| Only fires commit events if the text isn't empty
-}
viewTaskEditor :
    List (Attribute msg)
    ->
        { onCommit : msg
        , onCancel : Maybe msg
        , onChange : String -> msg
        , text : String
        }
    -> Element msg
viewTaskEditor attrs params =
    let
        cancelButton =
            case params.onCancel of
                Just onCancel_ ->
                    Icon.button [ padding 5 ] Icon.Cancel <| Just onCancel_

                Nothing ->
                    Element.none
    in
        row attrs
            [ Input.text []
                { onChange = params.onChange
                , text = params.text
                , placeholder = Just <| Input.placeholder [] (text "New task")
                , label =
                    Input.labelHidden "new task name"
                }
            , cancelButton
            , button [ padding 5 ]
                { onPress =
                    if String.isEmpty params.text then
                        Nothing
                    else
                        Just params.onCommit
                , label = Icon.el [] Icon.Add
                }
            ]


taskPadding : Attribute msg
taskPadding =
    padding 5


indent : Int -> Attribute msg
indent amt =
    paddingEach
        { top = 0
        , right = 0
        , bottom = 0
        , left = amt
        }


fill : Attribute msg
fill =
    Element.width Element.fill
