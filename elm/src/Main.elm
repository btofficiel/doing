module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, a, div, img, text, textarea)
import Html.Attributes exposing (class, href, placeholder, rows, src, value)
import Html.Events exposing (onClick, onInput)
import Route exposing (Route, parseUrl)
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
    , time : Maybe Int
    }


type alias Model =
    { route : Route
    , key : Nav.Key
    , taskString : String
    , tasks : List Task
    , colorMode : ColorMode
    }


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { route = parseUrl url
            , key = navKey
            , taskString = ""
            , tasks = []
            , colorMode = Light
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
        maybeTopTask =
            List.head model.tasks

        task =
            case maybeTopTask of
                Just t ->
                    t

                Nothing ->
                    { desc = "Error", time = Nothing }
    in
    [ div [ class "timer-container disabled" ]
        [ div [ class "elapsed-time" ] []
        ]
    , div [ class "menu-ctas" ]
        [ div [ class "menu-item" ]
            [ a [ href "/" ]
                [ img [ class "menu-cta", src (String.concat [ "assets/home-", getColor model.colorMode, ".svg" ]) ] []
                ]
            ]
        , div [ class "menu-item" ]
            [ a []
                [ img [ class "menu-cta", src (String.concat [ "assets/settings-", getColor model.colorMode, ".svg" ]) ] []
                ]
            ]
        ]
    , div [ class (String.concat [ "task-name ", getColor model.colorMode ]) ] [ text task.desc ]
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
            [ a []
                [ img [ class "menu-cta", src (String.concat [ "assets/settings-", getColor model.colorMode, ".svg" ]) ] []
                ]
            ]
        ]
    , div [ class "header" ] [ text "What's on your mind for today?" ]
    , div [ class "input" ]
        [ textarea
            [ class (getColor model.colorMode)
            , rows 16
            , placeholder "Write each task on its own line and press ENTER."
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
    Sub.none



--Update--


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url.Url
    | EnterTasks String
    | CreatePlaylist
    | MarkComplete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MarkComplete ->
            let
                taskString =
                    String.lines model.taskString
                        |> List.drop 1
                        |> String.join "\n"

                tasks =
                    List.drop 1 model.tasks
            in
            case String.length (String.trim taskString) > 0 of
                True ->
                    ( { model
                        | tasks = tasks
                        , taskString = taskString
                      }
                    , Cmd.none
                    )

                False ->
                    ( { model
                        | route = Route.Index
                        , tasks = tasks
                        , taskString = taskString
                      }
                    , Nav.pushUrl model.key "/"
                    )

        CreatePlaylist ->
            let
                tasks =
                    String.split "\n" model.taskString
                        |> List.map (\t -> { desc = t, time = Nothing })
            in
            case String.length (String.trim model.taskString) > 0 of
                True ->
                    ( { model
                        | tasks = tasks
                        , route = Route.Now
                      }
                    , Nav.pushUrl model.key "/now"
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
                    ( model, Nav.pushUrl model.key (Url.toString url) )

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
