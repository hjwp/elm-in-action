module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo, selectedUrl : String }


type alias Msg =
    { description : String, data : String }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick { description = "ClickedButton", data = "" } ]
            [ text "Suprise me!" ]
        , div
            [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl) ]
            []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick { description = "ClickedPhoto", data = thumb.url }
        ]
        []


update : Msg -> Model -> Model
update msg model =
    case msg.description of
        "ClickedPhoto" -> { model | selectedUrl = msg.data }
        "ClickedButton" -> { model | selectedUrl = "2.jpeg" }
        _ -> model



initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
