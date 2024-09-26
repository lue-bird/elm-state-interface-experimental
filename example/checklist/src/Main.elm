port module Main exposing (State, Todo, VisibilityFilter(..), main)

import Color
import Json.Decode
import Json.Encode
import Web


main : Web.Program State
main =
    Web.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


initialState : State
initialState =
    { todos = []
    , userInput = ""
    , visibilityFilter = AllVisible
    }


interface : State -> Web.Interface State
interface =
    \state ->
        [ ui state |> Web.domRender
        ]
            |> Web.interfaceBatch
            |> Web.interfaceFutureMap
                (\msg ->
                    case msg of
                        InputTextSubmitClicked ->
                            case state.userInput of
                                "" ->
                                    state

                                nonEmptyUserInput ->
                                    let
                                        newTodo =
                                            { content = nonEmptyUserInput
                                            , completed = False
                                            }

                                        newTodos =
                                            newTodo :: state.todos
                                    in
                                    { state | userInput = "", todos = newTodos }

                        InputTextChanged (Err _) ->
                            state

                        InputTextChanged (Ok str) ->
                            { state | userInput = str }

                        TodoRemoved todoId ->
                            let
                                filteredTodos =
                                    List.indexedMap Tuple.pair state.todos
                                        |> List.filter (\( i, _ ) -> i /= todoId)
                                        |> List.map (\( _, todo ) -> todo)
                            in
                            { state | todos = filteredTodos }

                        TodoCompletenessToggled todoIndex ->
                            { state
                                | todos =
                                    state.todos
                                        |> List.indexedMap
                                            (\i todo ->
                                                if i == todoIndex then
                                                    { todo | completed = not todo.completed }

                                                else
                                                    todo
                                            )
                            }

                        ResetAllToUncompletedClicked ->
                            { state
                                | todos =
                                    state.todos |> List.map (\todo -> { todo | completed = False })
                            }

                        VisibilityFilterSet newVisibilityFilter ->
                            { state | visibilityFilter = newVisibilityFilter }

                        RemoveCompleted ->
                            { state | todos = state.todos |> List.filter (\todo -> not todo.completed) }
                )


ui : State -> Web.DomNode Event
ui =
    \state ->
        Web.domElement "div"
            [ Web.domStyle "background-color" (Color.rgb 0 0 0 |> Color.toCssString)
            , Web.domStyle "color" (Color.rgb 1 1 1 |> Color.toCssString)
            , Web.domStyle "font-size" "2em"
            , Web.domStyle "padding-left" "80px"
            , Web.domStyle "padding-right" "80px"
            , Web.domStyle "position" "fixed"
            , Web.domStyle "top" "0"
            , Web.domStyle "right" "0"
            , Web.domStyle "bottom" "0"
            , Web.domStyle "left" "0"
            ]
            [ Web.domElement "div"
                [ Web.domStyle "max-width" "870px"
                , Web.domStyle "padding-top" "80px"
                ]
                [ Web.domElement "h1" [] [ Web.domText "todos" ]
                , Web.domElement "div"
                    []
                    [ todoNewItemInputUi state.userInput
                    , todoListInfoAndActionsUi state.todos state.visibilityFilter
                    , case state.todos of
                        [] ->
                            Web.domElement "div" [] []

                        todo0 :: todo1Up ->
                            Web.domElement "div"
                                [ Web.domStyle "padding" "34px 34px 0px 0px" ]
                                [ visibilityOptionsUi state.visibilityFilter
                                , todoListUi { todos = todo0 :: todo1Up, visibilityFilter = state.visibilityFilter }
                                ]
                    ]
                ]
            ]


buttonUi : List (Web.DomModifier ()) -> List (Web.DomNode ()) -> Web.DomNode ()
buttonUi modifiers subs =
    Web.domElement "button"
        ([ Web.domListenTo "click"
            |> Web.domModifierFutureMap (\_ -> ())
         , Web.domStyle "background-color" "transparent"
         , Web.domStyle "color" "inherit"
         , Web.domStyle "padding" "4px 13px"
         , Web.domStyle "text-align" "center"
         , Web.domStyle "font-size" "0.9em"
         , Web.domStyle "border-radius" "20px"
         , Web.domStyle "border-top" "none"
         , Web.domStyle "border-left" "none"
         , Web.domStyle "border-right" "none"
         , Web.domStyle "display" "inline-block"
         , Web.domStyle "border-bottom" ("2px solid " ++ (Color.rgba 1 1 1 0.2 |> Color.toCssString))
         ]
            ++ modifiers
        )
        subs


todoNewItemInputUi : String -> Web.DomNode Event
todoNewItemInputUi userInput =
    Web.domElement "div"
        []
        [ textInputUi InputTextChanged
            userInput
            [ Web.domAttribute "placeholder" "What needs to be done?"
            ]
        , buttonUi [] [ Web.domText "add" ]
            |> Web.domFutureMap (\() -> InputTextSubmitClicked)
        ]


textInputUi :
    (Result Json.Decode.Error String -> future)
    -> String
    -> List (Web.DomModifier future)
    -> Web.DomNode future
textInputUi toFuture inputValue modifiers =
    Web.domElement "input"
        ([ Web.domAttribute "type" "text"
         , Web.domStringProperty "value" inputValue
         , Web.domListenTo "input"
            |> Web.domModifierFutureMap
                (Json.Decode.decodeValue
                    (Json.Decode.field "target" (Json.Decode.field "value" Json.Decode.string))
                )
            |> Web.domModifierFutureMap toFuture
         , Web.domStyle "font-size" "1em"
         , Web.domStyle "background-color" "transparent"
         , Web.domStyle "border-bottom" ("2px solid " ++ (Color.rgba 1 1 1 0.5 |> Color.toCssString))
         , Web.domStyle "border-top" "none"
         , Web.domStyle "border-left" "none"
         , Web.domStyle "border-right" "none"
         , Web.domStyle "color" "inherit"
         ]
            ++ modifiers
        )
        []


visibilityFilterToString : VisibilityFilter -> String
visibilityFilterToString =
    \visibilityFilter ->
        case visibilityFilter of
            AllVisible ->
                "all"

            OnlyTodoVisible ->
                "only todo"

            OnlyCompletedVisible ->
                "only completed"


visibilityOptionsUi : VisibilityFilter -> Web.DomNode Event
visibilityOptionsUi currentVisibility =
    Web.domElement "div"
        []
        (Web.domElement "span"
            [ Web.domStyle "padding" "0px 2px 0px 0px" ]
            [ Web.domText ([ "showing ", currentVisibility |> visibilityFilterToString, ". Try also " ] |> String.concat)
            ]
            :: ([ AllVisible, OnlyTodoVisible, OnlyCompletedVisible ]
                    |> List.filter (\v -> v /= currentVisibility)
                    |> List.map (visibilityButtonUi currentVisibility)
               )
        )


visibilityButtonUi : VisibilityFilter -> VisibilityFilter -> Web.DomNode Event
visibilityButtonUi currentVisibilityFilter visibilityFilterToSetTo =
    buttonUi
        []
        [ Web.domText (visibilityFilterToSetTo |> visibilityFilterToString) ]
        |> Web.domFutureMap (\() -> VisibilityFilterSet visibilityFilterToSetTo)


todoListInfoAndActionsUi : List Todo -> VisibilityFilter -> Web.DomNode Event
todoListInfoAndActionsUi todos currentVisibility =
    case todos of
        [] ->
            Web.domText "no todos, yet."

        todo0 :: todo1Up ->
            let
                todoCount : Int
                todoCount =
                    todos |> List.filter (\todo -> not todo.completed) |> List.length
            in
            Web.domElement "div"
                []
                ((if (todo0 :: todo1Up) |> List.all .completed then
                    [ Web.domElement "div"
                        [ Web.domStyle "color" (Color.rgba 1 1 1 0.5 |> Color.toCssString)
                        ]
                        [ Web.domText "all completed." ]
                    , buttonUi [] [ Web.domText "reset all as to do" ]
                        |> Web.domFutureMap (\() -> ResetAllToUncompletedClicked)
                    ]

                  else
                    [ Web.domElement "div"
                        [ Web.domStyle "color" (Color.rgba 1 1 1 0.5 |> Color.toCssString)
                        ]
                        [ let
                            todoPluralized : String
                            todoPluralized =
                                case todoCount of
                                    1 ->
                                        "todo"

                                    _ ->
                                        "todos"
                          in
                          ([ todoCount |> String.fromInt, " ", todoPluralized, " left." ] |> String.concat)
                            |> Web.domText
                        ]
                    ]
                 )
                    ++ [ buttonUi
                            []
                            [ Web.domText "clear completed" ]
                            |> Web.domFutureMap (\() -> RemoveCompleted)
                       ]
                )


todoListUi : { todos : List Todo, visibilityFilter : VisibilityFilter } -> Web.DomNode Event
todoListUi state =
    let
        toListItem : ( Int, Todo ) -> Web.DomNode Event
        toListItem ( index, todo ) =
            Web.domElement "div"
                []
                [ buttonUi []
                    [ "✔" |> Web.domText
                    ]
                    |> Web.domFutureMap (\() -> TodoCompletenessToggled index)
                , buttonUi [ Web.domStyle "text-size" "0.9em", Web.domStyle "filter" "grayscale(100%)" ]
                    [ "💥" |> Web.domText ]
                    |> Web.domFutureMap (\() -> TodoRemoved index)
                , Web.domElement "span"
                    [ Web.domStyle "padding" "0px 0px 0px 20px"
                    , if todo.completed then
                        [ Web.domStyle "color" (Color.rgba 1 1 1 0.4 |> Color.toCssString)
                        , Web.domStyle "text-decoration" "line-through"
                        ]
                            |> Web.Dom.modifierBatch

                      else
                        Web.Dom.modifierNone
                    ]
                    [ todo.content |> Web.domText ]
                ]

        visibleTodos : List Todo
        visibleTodos =
            case state.visibilityFilter of
                AllVisible ->
                    state.todos

                OnlyTodoVisible ->
                    state.todos |> List.filter (\todo -> not todo.completed)

                OnlyCompletedVisible ->
                    state.todos |> List.filter .completed
    in
    Web.domElement "div"
        [ Web.domStyle "padding" "11px 11px 0px 0px" ]
        (visibleTodos
            |> List.indexedMap (\i todo -> ( i, todo ) |> toListItem)
        )


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event


type alias State =
    { todos : List Todo
    , userInput : String
    , visibilityFilter : VisibilityFilter
    }


type alias Todo =
    { completed : Bool
    , content : String
    }


type VisibilityFilter
    = AllVisible
    | OnlyTodoVisible
    | OnlyCompletedVisible


type Event
    = InputTextSubmitClicked
    | InputTextChanged (Result Json.Decode.Error String)
    | TodoRemoved Int
    | TodoCompletenessToggled Int
    | ResetAllToUncompletedClicked
    | VisibilityFilterSet VisibilityFilter
    | RemoveCompleted
