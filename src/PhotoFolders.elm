module PhotoFolders exposing (main)

import Browser
import Html exposing (..)
import Http
import Json.Decode


type alias Model =
    { selectedPhotoUrl : Maybe String
    }


type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)


view : Model -> Html Msg
view model =
    h1 [] [ text "The grooviest folders evar" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInitialModel (Ok m) ->
            let
                _ =
                    Debug.log "got initial model" m
            in
               ( m, Cmd.none )

        GotInitialModel (Err e) ->
            let
                _ =
                    Debug.log "error" e
            in
            ( model, Cmd.none )

        ClickedPhoto _ ->
            ( model, Cmd.none )


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        initialModel =
            { selectedPhotoUrl = Nothing }

        modelDecoder : Json.Decode.Decoder Model
        modelDecoder =
            Json.Decode.succeed initialModel -- TODO

        _ = Debug.log "init" "init"
    in
    ( initialModel
    , Http.get { url = "http://elm-in-action.com/folders/list", expect = Http.expectJson GotInitialModel modelDecoder }
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
