port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Events as Events
import Browser.Navigation as Nav
import Debug
import Html exposing (Html, a, div, img, span, text, textarea)
import Html.Attributes exposing (class, href, placeholder, rows, src, style, value)
import Html.Events exposing (onClick, onInput)
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


type alias Settings =
    { colorConfig : ColorSetting
    }


type alias Task =
    { desc : String
    , duration : Maybe Int
    }


type TimerState
    = Active TimerData
    | Paused TimerData
    | Inactive


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
    }


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
                        , timerState = Inactive
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
            }
    in
    ( model, Cmd.none )



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
                    , timerState = Inactive
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

                Inactive ->
                    "0%"

        taskDesc =
            case String.length task.desc > 49 of
                True ->
                    String.concat [ String.slice 0 49 task.desc, "…" ]

                False ->
                    task.desc
    in
    [ case task.timerState of
        Inactive ->
            div [ class (String.concat [ "timer-selector ", getColor model.colorMode ]) ]
                [ div [ class "timer-preset", onClick (TriggerSetTimer 5) ] [ text "5 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 15) ] [ text "15 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 25) ] [ text "25 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 45) ] [ text "45 min" ]
                , div [ class "timer-preset", onClick (TriggerSetTimer 60) ] [ text "60 min" ]
                ]

        _ ->
            div [ class (String.concat [ "timer-container ", getColor model.colorMode ]) ]
                [ div [ class "elapsed-time", style "width" remainingTime ] []
                ]
    , div [ class "menu-ctas" ]
        [ div [ class "menu-item" ]
            [ a [ href "/" ]
                [ img [ class "menu-cta", src (String.concat [ "assets/home-", getColor model.colorMode, ".svg" ]) ] []
                ]
            ]
        , div [ class "menu-item" ]
            [ span [ onClick ToggleColorMode ]
                [ img [ class "menu-cta", src (String.concat [ "assets/color-mode-", getColor model.colorMode, ".svg" ]) ] []
                ]
            ]
        ]
    , div [ class (String.concat [ "task-name ", getColor model.colorMode ]) ]
        [ text taskDesc ]
    , div [ class "task-cta" ]
        [ img
            [ class "task-cta"
            , src (String.concat [ "assets/task-done-", getColor model.colorMode, ".svg" ])
            , onClick MarkComplete
            ]
            []
        ]
    ]


playlistView : Model -> List (Html Msg)
playlistView model =
    [ div [ class "timer-container disabled" ]
        [ div [ class "elapsed-time" ] []
        ]
    , div [ class "menu-ctas" ]
        [ div [ class "menu-item" ]
            [ div [ class "menu-item" ]
                [ span [ onClick ToggleColorMode ]
                    [ img [ class "menu-cta", src (String.concat [ "assets/color-mode-", getColor model.colorMode, ".svg" ]) ] []
                    ]
                ]
            ]
        ]
    , div [ class "header" ] [ text "What's on your mind for today?" ]
    , div [ class "input" ]
        [ textarea
            [ class (getColor model.colorMode)
            , rows 16
            , placeholder "Write each task on its own line."
            , value model.taskString
            , onInput EnterTasks
            ]
            []
        ]
    , div [ class "task-cta" ]
        [ img
            [ class "task-cta"
            , src (String.concat [ "assets/create-playlist-", getColor model.colorMode, ".svg" ])
            , onClick CreatePlaylist
            ]
            []
        ]
    ]


view : Model -> Document Msg
view model =
    { title = "Doing"
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



--Subscriptions--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrame Tick
        , Events.onVisibilityChange VisibilityChange
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                                    Active { ts | remaining = remaining }

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
                                            Active { ts | remaining = remaining }

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
                            Just { task = t, timerState = Inactive }

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
                    , updateTaskList taskString
                    )

                False ->
                    ( { model
                        | route = Route.Index
                        , tasks = tasks
                        , taskString = taskString
                      }
                    , Cmd.batch [ Nav.pushUrl model.key "/", updateTaskList taskString ]
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
                            Just { task = t, timerState = Inactive }

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
                    , Cmd.batch [ Nav.pushUrl model.key "/now", updateTaskList model.taskString ]
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
