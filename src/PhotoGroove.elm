port module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Encode
import Random
import String.Format as Format


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type ThumbnailSize
    = Small
    | Medium
    | Large


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
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int


type alias FilterConfig =
    { url : String
    , filters : List { name : String, amount : Float }
    }


port setFilter : FilterConfig -> Cmd msg


setFilterConfig : Model -> Cmd msg
setFilterConfig model =
    case model.status of
        Loaded _ url ->
            setFilter
                { url = urlPrefix ++ "large/" ++ url
                , filters =
                    [ { name = "hue", amount = toFloat model.hue / 11 }
                    , { name = "ripple", amount = toFloat model.ripple / 11 }
                    , { name = "noise", amount = toFloat model.noise / 11 }
                    ]
                }

        _ ->
            Cmd.none


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                [ viewLoaded photos selectedUrl model ]

            Loading ->
                [ text "Loading..." ]

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Photo -> String -> Model -> Html Msg
viewLoaded photos selectedUrl model =
    div []
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Suprise me!" ]
        , div [ class "filters" ]
            [ viewFilter "Hue" model.hue SlidHue
            , viewFilter "Ripple" model.ripple SlidRipple
            , viewFilter "Noise" model.noise SlidNoise
            ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div
            [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail selectedUrl) photos)
        , canvas [ id "main-canvas", class "large" ] []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title ("{{ title }} [{{ size }} KB]" |> Format.namedValue "title" thumb.title |> Format.namedValue "size" (String.fromInt thumb.size))
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


viewFilter : String -> Int -> (Int -> Msg) -> Html Msg
viewFilter name magnitude toMsg =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Html.Attributes.max "11"
            , Html.Attributes.property "val" (Json.Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
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


handlePhotoResponse : Result Http.Error String -> Msg
handlePhotoResponse result =
    case result of
        Ok str ->
            case
                Json.Decode.decodeString (Json.Decode.list photoDecoder) str
            of
                Ok photos ->
                    GotPhotos photos

                Err _ ->
                    GotPhotos []

        Err _ ->
            GotPhotos []


photoDecoder : Json.Decode.Decoder Photo
photoDecoder =
    Json.Decode.map3 Photo
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "size" Json.Decode.int)
        (Json.Decode.map (Maybe.withDefault "(untitled)") (Json.Decode.maybe (Json.Decode.field "title" Json.Decode.string)))


updateChosenPhoto : Model -> String -> Model
updateChosenPhoto model url =
    case model.status of
        Loaded photos _ ->
            { model | status = Loaded photos url }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPhotos ((firstPhoto :: _) as photos) ->
            let
                newModel =
                    { model | status = Loaded photos firstPhoto.url }

                _ =
                    Debug.log "Got photos" photos
            in
            ( newModel, setFilterConfig newModel )

        GotPhotos [] ->
            ( { model | status = Errored "no photos found" }, Cmd.none )

        ClickedPhoto url ->
            let
                newModel =
                    updateChosenPhoto model url
            in
            ( newModel, setFilterConfig newModel )

        GotRandomPhoto photo ->
            let
                newModel =
                    updateChosenPhoto model photo.url
            in
            ( newModel, setFilterConfig newModel )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    ( model
                    , Random.generate GotRandomPhoto <|
                        Random.uniform firstPhoto otherPhotos
                    )

                _ ->
                    ( { model | status = Errored "no photos" }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        SlidHue val ->
            let
                newModel =
                    { model | hue = val }
            in
            ( newModel, setFilterConfig newModel )

        SlidNoise val ->
            let
                newModel =
                    { model | noise = val }
            in
            ( newModel, setFilterConfig newModel )

        SlidRipple val ->
            let
                newModel =
                    { model | ripple = val }
            in
            ( newModel, setFilterConfig newModel )


onSlide : (Int -> Msg) -> Attribute Msg
onSlide toMsg =
    let
        valueFinder : Json.Decode.Decoder Int
        valueFinder =
            Json.Decode.at [ "detail", "userSlidTo" ] Json.Decode.int

        toMsgDecoder : Json.Decode.Decoder Msg
        toMsgDecoder =
            Json.Decode.map toMsg valueFinder
    in
    Html.Events.on "slide" toMsgDecoder


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    , hue = 3
    , ripple = 3
    , noise = 3
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { expect = Http.expectString handlePhotoResponse
        , url = urlPrefix ++ "/photos/list.json"
        }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
