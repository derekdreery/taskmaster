module Main exposing (..)

import Set
import Debug
import Browser
import Element exposing (Element, Attribute, paddingEach, text, rgb, el, none, column, px)
import Element.Events exposing (onClick)
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Tasks exposing (Tasklist, EditingTask, EditType(..), Msg(..))


---- MODEL ----


type alias Model =
    { tasks : Tasklist
    , editingTask : Maybe EditingTask
    }


init : ( Model, Cmd Msg )
init =
    ( { tasks = Tasks.dummyTasks
      , editingTask = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | TaskMsg Tasks.Msg


{-| I'm writing the whole update function here because you can't import the model definition into
submodules.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TaskMsg (ToggleExpandTask path) ->
            let
                tasks =
                    Tasks.toggleExpand path model.tasks
            in
                ( { model | tasks = tasks }, Cmd.none )

        TaskMsg CommitEditTask ->
            case model.editingTask of
                Just { editType, path, text } ->
                    case editType of
                        Add ->
                            ( { model
                                | tasks = Tasks.addTask text path model.tasks
                                , editingTask = Nothing
                              }
                            , Cmd.none
                            )

                        Edit ->
                            ( { model
                                | tasks = Tasks.updateTask text path model.tasks
                                , editingTask = Nothing
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        TaskMsg (StartEditTask name path) ->
            ( { model
                | editingTask =
                    Just
                        { editType = Edit
                        , path = path
                        , text = name
                        }
              }
            , Cmd.none
            )

        TaskMsg (StartAddTask path) ->
            ( { model
                | editingTask =
                    Just
                        { editType = Add
                        , path = path
                        , text = ""
                        }
              }
            , Cmd.none
            )

        TaskMsg CancelEditTask ->
            ( { model | editingTask = Nothing }, Cmd.none )

        TaskMsg (UpdateEditText newText) ->
            ( { model | editingTask = Maybe.map (Tasks.updateEditText newText) model.editingTask }, Cmd.none )

        TaskMsg (DeleteTask path) ->
            ( { model | tasks = Tasks.removeTask path model.tasks }, Cmd.none )

        TaskMsg (TaskToLeaf path) ->
            ( model, Cmd.none )

        TaskMsg (TaskToBranch path) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ Element.width Element.fill ]
        [ el
            [ Background.color <| rgb 0 0 0
            , Element.width Element.fill
            , Font.color <| rgb 1 1 1
            , Font.center
            , Element.padding 10
            ]
            (text "My Tasks")
        , el [ Element.height <| px 10 ] none
        , Tasks.viewTasklist [] model.editingTask [] model.tasks
            |> Element.map TaskMsg
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view =
            \m ->
                Element.layout
                    [ Element.width Element.fill
                    , Font.family
                        [ Font.typeface "Roboto"
                        , Font.sansSerif
                        ]
                    ]
                    (view m)
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
