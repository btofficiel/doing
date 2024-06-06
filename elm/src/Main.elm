module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (div, img, text, textarea)
import Html.Attributes exposing (class, placeholder, rows, src)
import Url



--Model--


type alias Model =
    { key : Nav.Key
    , taskString : String
    , url : Url.Url
    }


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { key = navKey
            , taskString = ""
            , url = url
            }
    in
    ( model, Cmd.none )



--View--


view : Model -> Document Msg
view model =
    { title = "Doing"
    , body =
        [ div [ class "content light" ]
            [ div [ class "spotlight light" ] []
            , div [ class "timer-container disabled" ]
                [ div [ class "elapsed-time" ] []
                ]
            , div [ class "menu-ctas" ]
                [ div [ class "menu-item" ]
                    [ img [ class "menu-cta", src "assets/settings-light.svg" ] []
                    ]
                ]
            , div [ class "header" ] [ text "What's on your mind for today?" ]
            , div [ class "input" ]
                [ textarea [ class "light", rows 16, placeholder "Write each task on its own line and press ENTER." ] []
                ]
            , div [ class "task-cta" ]
                [ img [ class "task-cta", src "assets/create-playlist-light.svg" ] []
                ]
            ]
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
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
