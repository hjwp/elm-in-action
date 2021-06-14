module PhotoGroove exposing (main)

import Browser
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotPhotos (List Photo)
    | GotRandomPhoto Photo


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status, chosenSize : ThumbnailSize }


type ThumbnailSize
    = Small
    | Medium
    | Large


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                [ viewLoaded photos selectedUrl model.chosenSize ]

            Loading ->
                [ text "Loading..." ]

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Photo -> String -> ThumbnailSize -> Html Msg
viewLoaded photos selectedUrl chosenSize =
    div []
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Suprise me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div
            [ id "thumbnails", class (sizeToString chosenSize) ]
            (List.map (viewThumbnail selectedUrl) photos)
        , img
            [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ]
            []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status


handlePhotoResponse : Result Http.Error String -> Msg
handlePhotoResponse result =
    case result of
        Ok str ->
            GotPhotos (List.map (\url -> (Photo url 1 "tbc")) (String.split "," str))

        Err _ ->
            GotPhotos []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPhotos ((firstPhoto :: _) as photos) ->
            ( { model | status = Loaded photos firstPhoto.url }
            , Cmd.none
            )

        GotPhotos [] ->
            ( { model | status = Errored "no photos found" }, Cmd.none )

        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    ( model
                    , Random.generate GotRandomPhoto
                        (Random.uniform firstPhoto otherPhotos)
                    )

                _ ->
                    ( { model | status = Errored "no photos" }, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { expect = Http.expectString handlePhotoResponse
        , url = urlPrefix ++ "/photos/list"
        }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
