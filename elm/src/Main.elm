port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Debug
import Dict
import Html exposing (Html, a, button, div, img, span, text, textarea)
import Html.Attributes exposing (class, href, id, placeholder, rows, src, style, tabindex, title, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Route exposing (Route, parseUrl)
import Task
import Time
import Url



--Model--


type ColorSetting
    = LightScheme
    | DarkScheme
    | Auto


type ColorMode
    = Light
    | Dark


type ShortcutKey
    = CmdOrCtrl
    | Enter
    | Spacebar
    | EorI


type alias Settings =
    { colorConfig : ColorSetting
    }


type alias Task =
    { desc : String
    , duration : Maybe Int
    }


type PresetVisibility
    = ShowPresets (Maybe Time.Posix)
    | ShowPresetsExtra Time.Posix
    | HidePresets


type TimerState
    = Active TimerData
    | Paused TimerData
    | Over
    | Inactive PresetVisibility


type alias TimerData =
    { start : Time.Posix
    , end : Time.Posix
    , remaining : Float
    }


type alias CurrentTask =
    { task : Task
    , timerState : TimerState
    }


type alias Model =
    { route : Route
    , key : Nav.Key
    , taskString : String
    , tasks : List Task
    , colorMode : ColorMode
    , currentTask : Maybe CurrentTask
    , keyHeld : Maybe ShortcutKey
    , rows : Int
    }


activateTimerPreset =
    Task.perform ActivateTimerPreset Time.now


focusOnTextbox =
    Task.attempt (\_ -> NoOp) (Dom.focus "textbox")


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        taskString =
            case flags of
                Just str ->
                    String.split "\n" str
                        |> List.map (\t -> String.trim t)
                        |> List.filter (\t -> String.length t > 0)
                        |> String.join "\n"

                Nothing ->
                    ""

        tasks =
            String.split "\n" taskString
                |> List.map (\t -> { desc = t, duration = Nothing })

        currentTask =
            List.head tasks

        currentTaskState =
            case currentTask of
                Just t ->
                    Just
                        { task = t
                        , timerState = Inactive (ShowPresets Nothing)
                        }

                Nothing ->
                    Nothing

        model =
            { route = parseUrl url
            , key = navKey
            , taskString = taskString
            , tasks = tasks
            , colorMode = Light
            , currentTask = currentTaskState
            , keyHeld = Nothing
            , rows = 16
            }

        maybeActivateTimerPreset =
            case currentTaskState of
                Just _ ->
                    [ activateTimerPreset ]

                Nothing ->
                    []

        cmds =
            List.append [ Task.perform GetInitialViewport Dom.getViewport ] maybeActivateTimerPreset
                |> List.append [ focusOnTextbox ]
    in
    ( model, Cmd.batch cmds )



--View--


getColor : ColorMode -> String
getColor colorMode =
    case colorMode of
        Light ->
            "light"

        Dark ->
            "dark"


focusView : Model -> List (Html Msg)
focusView model =
    let
        task =
            case model.currentTask of
                Just t ->
                    { desc = t.task.desc
                    , timerState = t.timerState
                    }

                Nothing ->
                    { desc = "Sorry, some error occurred"
                    , timerState = Inactive (ShowPresets Nothing)
                    }

        remainingTime =
            case task.timerState of
                Active ts ->
                    ts.remaining
                        |> String.fromFloat
                        |> (\r -> String.concat [ r, "%" ])

                Paused ts ->
                    ts.remaining
                        |> String.fromFloat
                        |> (\r -> String.concat [ r, "%" ])

                _ ->
                    "0%"

        taskDesc =
            case String.length task.desc > 49 of
                True ->
                    String.concat [ String.slice 0 49 task.desc, "…" ]

                False ->
                    task.desc

        showTimerButton =
            case task.timerState of
                Inactive HidePresets ->
                    [ div [ class "menu-item" ]
                        [ button [ title "Add additional time", onClick TriggerShowTimerPresets ]
                            [ img [ class "menu-cta", src (String.concat [ "assets/timer-", getColor model.colorMode, ".svg" ]) ] []
                            ]
                        ]
                    ]

                Over ->
                    [ div [ class "menu-item" ]
                        [ button [ title "Add additional time", onClick TriggerShowTimerPresets ]
                            [ img [ class "menu-cta", src (String.concat [ "assets/timer-", getColor model.colorMode, ".svg" ]) ] []
                            ]
                        ]
                    ]

                _ ->
                    []
    in
    [ case task.timerState of
        Inactive (ShowPresets _) ->
            div [ class (String.concat [ "timer-selector ", getColor model.colorMode ]) ]
                [ div [ class "timer-preset", onClick (TriggerSetTimer 5) ] [ text "5 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 15) ] [ text "15 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 25) ] [ text "25 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 45) ] [ text "45 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 60) ] [ text "60 min" ]
                ]

        Inactive (ShowPresetsExtra _) ->
            div [ class (String.concat [ "timer-selector ", getColor model.colorMode ]) ]
                [ div [ class "timer-preset", onClick (TriggerSetTimer 5) ] [ text "+5 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 15) ] [ text "+15 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 25) ] [ text "+25 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 45) ] [ text "+45 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 60) ] [ text "+60 min" ]
                ]

        Inactive HidePresets ->
            div [ class "timer-container disabled" ]
                [ div [ class "elapsed-time", style "width" remainingTime ] []
                ]

        _ ->
            div [ class (String.concat [ "timer-container ", getColor model.colorMode ]) ]
                [ div [ class "elapsed-time", style "width" remainingTime ] []
                ]
    , div [ class "menu-ctas" ]
        (showTimerButton
            ++ [ div [ class "menu-item" ]
                    [ button [ title "Edit tasklist", onClick EditPlaylist ]
                        [ img [ class "menu-cta", src (String.concat [ "assets/home-", getColor model.colorMode, ".svg" ]) ] []
                        ]
                    ]
               , div [ class "menu-item" ]
                    [ a [ href "https://github.com/btofficiel/doing" ]
                        [ img [ class "menu-cta", src (String.concat [ "assets/github-", getColor model.colorMode, ".svg" ]) ] []
                        ]
                    ]
               , div [ class "menu-item" ]
                    [ button [ title "Toggle colormode", onClick ToggleColorMode ]
                        [ img [ class "menu-cta", src (String.concat [ "assets/color-mode-", getColor model.colorMode, ".svg" ]) ] []
                        ]
                    ]
               ]
        )
    , div [ class (String.concat [ "task-name ", getColor model.colorMode ]) ]
        [ text taskDesc ]
    , div [ class "task-cta" ]
        [ button [ title "Mark Done", onClick MarkComplete ]
            [ img
                [ class "task-cta"
                , src (String.concat [ "assets/task-done-", getColor model.colorMode, ".svg" ])
                ]
                []
            ]
        ]
    ]


playlistView : Model -> List (Html Msg)
playlistView model =
    [ div [ class "timer-container disabled" ]
        [ div [ class "elapsed-time" ] []
        ]
    , div [ class "menu-ctas" ]
        [ div [ class "menu-item" ]
            [ a [ href "https://github.com/btofficiel/doing", tabindex 3 ]
                [ img [ class "menu-cta", src (String.concat [ "assets/github-", getColor model.colorMode, ".svg" ]) ] []
                ]
            ]
        , div [ class "menu-item" ]
            [ button [ title "Toggle Colormode", onClick ToggleColorMode, tabindex 4 ]
                [ img [ class "menu-cta", src (String.concat [ "assets/color-mode-", getColor model.colorMode, ".svg" ]) ] []
                ]
            ]
        ]
    , div [ class "header" ] [ text "What's on your mind for today?" ]
    , div [ class "input" ]
        [ textarea
            [ class (getColor model.colorMode)
            , id "textbox"
            , tabindex 1
            , rows model.rows
            , placeholder "Write each intention on its own line."
            , value model.taskString
            , onInput EnterTasks
            ]
            []
        ]
    , div [ class "task-cta" ]
        [ button
            [ title "Create Tasklist"
            , class "task-cta"
            , tabindex 2
            , onClick CreatePlaylist
            ]
            [ img
                [ class "task-cta"
                , src (String.concat [ "assets/create-playlist-", getColor model.colorMode, ".svg" ])
                ]
                []
            ]
        ]
    ]


view : Model -> Document Msg
view model =
    { title = "doing.is - Single tasking made easy!"
    , body =
        [ div [ class (String.concat [ "content ", getColor model.colorMode ]) ]
            (List.concat
                [ [ div [ class (String.concat [ "spotlight ", getColor model.colorMode ]) ] []
                  ]
                , case model.route of
                    Route.Now ->
                        focusView model

                    _ ->
                        playlistView model
                ]
            )
        ]
    }



--Key Decoders--


keyDecoder : Decode.Decoder (Maybe ShortcutKey)
keyDecoder =
    Decode.map toMaybeShortcut (Decode.field "keyCode" Decode.int)


toMaybeShortcut : Int -> Maybe ShortcutKey
toMaybeShortcut keyCode =
    let
        keyHeld =
            Dict.fromList
                [ ( 17, CmdOrCtrl )
                , ( 91, CmdOrCtrl )
                , ( 13, Enter )
                , ( 32, Spacebar )
                , ( 73, EorI )
                , ( 69, EorI )
                ]
    in
    Dict.get keyCode keyHeld



--Subscriptions--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrame Tick
        , Events.onVisibilityChange VisibilityChange
        , Events.onResize (\w h -> AdjustRows (toFloat w) (toFloat h))
        , Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Events.onKeyUp (Decode.map KeyUp keyDecoder)
        , Time.every 1000 HideTimerPresetVisibiltiy
        ]


port updateTaskList : String -> Cmd msg



--Update--


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url.Url
    | EnterTasks String
    | CreatePlaylist
    | MarkComplete
    | TriggerSetTimer Int
    | SetTimer Int Time.Posix
    | RecalibrateTimer Time.Posix
    | Tick Time.Posix
    | ToggleColorMode
    | VisibilityChange Events.Visibility
    | GetInitialViewport Dom.Viewport
    | AdjustRows Float Float
    | KeyDown (Maybe ShortcutKey)
    | KeyUp (Maybe ShortcutKey)
    | TriggerShowTimerPresets
    | HideTimerPresetVisibiltiy Time.Posix
    | ActivateTimerPreset Time.Posix
    | EditPlaylist
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInitialViewport vp ->
            let
                cmd =
                    Task.perform (\_ -> AdjustRows vp.viewport.width vp.viewport.height) (Task.succeed Nothing)
            in
            ( model, cmd )

        EditPlaylist ->
            ( model, Cmd.batch [ Nav.pushUrl model.key "/", focusOnTextbox ] )

        TriggerShowTimerPresets ->
            ( model, Task.perform ActivateTimerPreset Time.now )

        ActivateTimerPreset posix ->
            case model.currentTask of
                Just t ->
                    case t.timerState of
                        Inactive (ShowPresets Nothing) ->
                            let
                                newCurrentTask =
                                    Just { t | timerState = Inactive (ShowPresets (Just posix)) }
                            in
                            ( { model
                                | currentTask = newCurrentTask
                              }
                            , Cmd.none
                            )

                        Inactive HidePresets ->
                            let
                                newCurrentTask =
                                    Just { t | timerState = Inactive (ShowPresetsExtra posix) }
                            in
                            ( { model
                                | currentTask = newCurrentTask
                              }
                            , Cmd.none
                            )

                        Over ->
                            let
                                newCurrentTask =
                                    Just { t | timerState = Inactive (ShowPresetsExtra posix) }
                            in
                            ( { model
                                | currentTask = newCurrentTask
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        HideTimerPresetVisibiltiy posix ->
            case model.route of
                Route.Now ->
                    case model.currentTask of
                        Just t ->
                            case t.timerState of
                                Inactive (ShowPresets (Just mpsx)) ->
                                    let
                                        current =
                                            Time.posixToMillis posix

                                        start =
                                            Time.posixToMillis mpsx

                                        elapsed =
                                            current - start

                                        newCurrentTask =
                                            case elapsed >= 60000 of
                                                True ->
                                                    Just { t | timerState = Inactive HidePresets }

                                                False ->
                                                    Just t
                                    in
                                    ( { model
                                        | currentTask = newCurrentTask
                                      }
                                    , Cmd.none
                                    )

                                Inactive (ShowPresetsExtra mpsx) ->
                                    let
                                        current =
                                            Time.posixToMillis posix

                                        start =
                                            Time.posixToMillis mpsx

                                        elapsed =
                                            current - start

                                        newCurrentTask =
                                            case elapsed >= 60000 of
                                                True ->
                                                    Just { t | timerState = Inactive HidePresets }

                                                False ->
                                                    Just t
                                    in
                                    ( { model
                                        | currentTask = newCurrentTask
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyDown key ->
            case model.route of
                Route.Index ->
                    case key of
                        Just CmdOrCtrl ->
                            ( { model
                                | keyHeld = key
                              }
                            , Cmd.none
                            )

                        Just Enter ->
                            ( model, Cmd.none )

                        _ ->
                            ( { model
                                | keyHeld = Nothing
                              }
                            , Cmd.none
                            )

                Route.Now ->
                    case key of
                        Just CmdOrCtrl ->
                            ( { model
                                | keyHeld = key
                              }
                            , Cmd.none
                            )

                        Just EorI ->
                            ( model, Cmd.none )

                        Just Spacebar ->
                            ( model, Cmd.none )

                        _ ->
                            ( { model
                                | keyHeld = Nothing
                              }
                            , Cmd.none
                            )

                _ ->
                    ( { model
                        | keyHeld = Nothing
                      }
                    , Cmd.none
                    )

        KeyUp key ->
            case model.route of
                Route.Index ->
                    case model.keyHeld of
                        Just CmdOrCtrl ->
                            case key of
                                Just CmdOrCtrl ->
                                    ( { model
                                        | keyHeld = Nothing
                                      }
                                    , Cmd.none
                                    )

                                Just Enter ->
                                    ( { model
                                        | keyHeld = Nothing
                                      }
                                    , Task.perform (\_ -> CreatePlaylist) (Task.succeed Nothing)
                                    )

                                _ ->
                                    ( { model
                                        | keyHeld = Nothing
                                      }
                                    , Cmd.none
                                    )

                        _ ->
                            ( model, Cmd.none )

                Route.Now ->
                    case model.keyHeld of
                        Just CmdOrCtrl ->
                            case key of
                                Just CmdOrCtrl ->
                                    ( { model
                                        | keyHeld = Nothing
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( { model
                                        | keyHeld = Nothing
                                      }
                                    , Cmd.none
                                    )

                        Nothing ->
                            case key of
                                Just Spacebar ->
                                    ( { model
                                        | keyHeld = Nothing
                                      }
                                    , Task.perform (\_ -> MarkComplete) (Task.succeed Nothing)
                                    )

                                Just EorI ->
                                    ( { model
                                        | keyHeld = Nothing
                                      }
                                    , Task.perform (\_ -> EditPlaylist) (Task.succeed Nothing)
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AdjustRows x y ->
            let
                rows =
                    y
                        |> (\height -> height / 1070)
                        |> (\frac -> frac * 16)
                        |> floor
            in
            ( { model
                | rows = rows
              }
            , Cmd.none
            )

        RecalibrateTimer posix ->
            case model.currentTask of
                Just t ->
                    case t.timerState of
                        Paused ts ->
                            let
                                start =
                                    Time.posixToMillis ts.start

                                end =
                                    Time.posixToMillis ts.end

                                current =
                                    Time.posixToMillis posix

                                elapsed =
                                    current - start

                                duration =
                                    end - start

                                remaining =
                                    (duration - elapsed)
                                        |> toFloat
                                        |> (\x -> x / toFloat duration)
                                        |> (\x -> x * 100)
                                        |> max 0

                                newTimerState =
                                    case remaining > 0 of
                                        True ->
                                            Active { ts | remaining = remaining }

                                        False ->
                                            Over

                                newCurrentTask =
                                    Just { task = t.task, timerState = newTimerState }
                            in
                            ( { model
                                | currentTask = newCurrentTask
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        VisibilityChange visibility ->
            case model.currentTask of
                Just t ->
                    case t.timerState of
                        Active ts ->
                            case visibility of
                                Events.Hidden ->
                                    let
                                        newCurrentTask =
                                            Just { t | timerState = Paused ts }
                                    in
                                    ( { model
                                        | currentTask = newCurrentTask
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        Paused ts ->
                            case visibility of
                                Events.Visible ->
                                    ( model, Task.perform RecalibrateTimer Time.now )

                                _ ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleColorMode ->
            case model.colorMode of
                Light ->
                    ( { model
                        | colorMode = Dark
                      }
                    , Cmd.none
                    )

                Dark ->
                    ( { model
                        | colorMode = Light
                      }
                    , Cmd.none
                    )

        Tick posix ->
            case model.route of
                Route.Now ->
                    case model.currentTask of
                        Just t ->
                            case t.timerState of
                                Active ts ->
                                    let
                                        start =
                                            Time.posixToMillis ts.start

                                        end =
                                            Time.posixToMillis ts.end

                                        current =
                                            Time.posixToMillis posix

                                        elapsed =
                                            current - start

                                        duration =
                                            end - start

                                        remaining =
                                            (duration - elapsed)
                                                |> toFloat
                                                |> (\x -> x / toFloat duration)
                                                |> (\x -> x * 100)
                                                |> max 0

                                        newTimerState =
                                            case remaining > 0 of
                                                True ->
                                                    Active { ts | remaining = remaining }

                                                False ->
                                                    Over

                                        newCurrentTask =
                                            Just { task = t.task, timerState = newTimerState }
                                    in
                                    ( { model
                                        | currentTask = newCurrentTask
                                      }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TriggerSetTimer min ->
            case model.currentTask of
                Just t ->
                    ( model, Task.perform (SetTimer min) Time.now )

                Nothing ->
                    ( model, Cmd.none )

        SetTimer min now ->
            case model.currentTask of
                Just t ->
                    let
                        minToMillis =
                            min * 60 * 1000

                        end =
                            now
                                |> Time.posixToMillis
                                |> (\p -> p + minToMillis)
                                |> Time.millisToPosix

                        remaining =
                            100

                        currentTask =
                            { task =
                                { desc = t.task.desc
                                , duration = Just min
                                }
                            , timerState = Active { start = now, end = end, remaining = remaining }
                            }
                    in
                    ( { model
                        | currentTask = Just currentTask
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        MarkComplete ->
            let
                taskString =
                    String.lines model.taskString
                        |> List.drop 1
                        |> String.join "\n"

                tasks =
                    List.drop 1 model.tasks

                currentTask =
                    List.head tasks

                newCurrentTaskState =
                    case currentTask of
                        Just t ->
                            Just { task = t, timerState = Inactive (ShowPresets Nothing) }

                        Nothing ->
                            Nothing
            in
            case String.length (String.trim taskString) > 0 of
                True ->
                    ( { model
                        | tasks = tasks
                        , taskString = taskString
                        , currentTask = newCurrentTaskState
                      }
                    , Cmd.batch [ updateTaskList taskString, activateTimerPreset ]
                    )

                False ->
                    ( { model
                        | route = Route.Index
                        , tasks = tasks
                        , taskString = taskString
                      }
                    , Cmd.batch [ Nav.pushUrl model.key "/", updateTaskList taskString, focusOnTextbox ]
                    )

        CreatePlaylist ->
            let
                taskString =
                    String.split "\n" model.taskString
                        |> List.map (\t -> String.trim t)
                        |> List.filter (\t -> String.length t > 0)
                        |> String.join "\n"

                tasks =
                    String.split "\n" taskString
                        |> List.map (\t -> { desc = t, duration = Nothing })

                currentTask =
                    List.head tasks

                newCurrentTaskState =
                    case currentTask of
                        Just t ->
                            Just { task = t, timerState = Inactive (ShowPresets Nothing) }

                        Nothing ->
                            Nothing
            in
            case String.length (String.trim model.taskString) > 0 of
                True ->
                    ( { model
                        | taskString = taskString
                        , tasks = tasks
                        , currentTask = newCurrentTaskState
                        , route = Route.Now
                      }
                    , Cmd.batch [ Nav.pushUrl model.key "/now", updateTaskList model.taskString, activateTimerPreset ]
                    )

                False ->
                    ( model, Cmd.none )

        EnterTasks tasks ->
            ( { model
                | taskString = tasks
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model
                        | route = parseUrl url
                      }
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = parseUrl url }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
