module PhotoFolders exposing (main)

import Browser
import Html exposing (..)
import Http


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
    ( model, Cmd.none )


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { selectedPhotoUrl = Nothing }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
