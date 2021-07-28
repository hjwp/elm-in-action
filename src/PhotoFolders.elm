module PhotoFolders exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    }


type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com"


view : Model -> Html Msg
view model =
    let
        photoAt url =
            Dict.get url model.photos
    in
    div [ class "content" ]
        [ div [ class "selected-photo" ]
            [ case Maybe.andThen photoAt model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
            ]
        ]


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src (urlPrefix ++ "/photos/" ++ photo.url ++ "/full") ] []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "related" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photos"
        , onClick (ClickedPhoto url)
        , src (urlPrefix ++ "/photos/" ++ url ++ "/thumb")
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err e) ->
            let
                _ =
                    Debug.log "error" e
            in
            ( model, Cmd.none )

        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        initialModel =
            { selectedPhotoUrl = Nothing, photos = Dict.empty }

        modelDecoder : Json.Decode.Decoder Model
        modelDecoder =
            Json.Decode.succeed initialModel

        -- TODO
        _ =
            Debug.log "init" "init"
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
