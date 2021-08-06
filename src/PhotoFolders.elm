module PhotoFolders exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import PhotoGroove exposing (photoDecoder)


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
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


type Folder
    = Folder
        { name : String
        , photoUrls : List String
        , subFolders : List Folder
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
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder model.root
            ]
        , div [ class "selected-photo" ]
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


viewFolder : Folder -> Html Msg
viewFolder (Folder folder) =
    div [ class "folder" ]
        [ label [] [ text folder.name ]
        , div [ class "subfolders" ] (List.map viewFolder folder.subFolders)
        ]


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
        initialModel : Model
        initialModel =
            { selectedPhotoUrl = Nothing
            , photos = Dict.empty
            , root = Folder { name = "empty", photoUrls = [], subFolders = [] }
            }

        modelDecoder : Json.Decode.Decoder Model
        modelDecoder =
            Json.Decode.succeed
                { selectedPhotoUrl = Just "trevi"
                , photos =
                    Dict.fromList
                        [ ( "trevi"
                          , { title = "Trevi"
                            , relatedUrls = [ "coli", "fresco" ]
                            , size = 34
                            , url = "trevi"
                            }
                          )
                        , ( "fresco"
                          , { title = "Fresco"
                            , relatedUrls = [ "trevi" ]
                            , size = 46
                            , url = "fresco"
                            }
                          )
                        , ( "coli"
                          , { title = "Coli"
                            , relatedUrls = [ "trevi", "fresco" ]
                            , size = 46
                            , url = "coli"
                            }
                          )
                        ]
                , root =
                    Folder
                        { name = "Photos"
                        , photoUrls = []
                        , subFolders =
                            [ Folder
                                { name = "outdoors"
                                , photoUrls = []
                                , subFolders = []
                                }
                            , Folder
                                { name = "indoors"
                                , photoUrls = [ "fresco" ]
                                , subFolders = []
                                }
                            , Folder
                                { name = "2017"
                                , photoUrls = []
                                , subFolders =
                                    [ Folder { name = "outdoors", photoUrls = [], subFolders = [] }
                                    , Folder { name = "indoors", photoUrls = [], subFolders = [] }
                                    ]
                                }
                            ]
                        }
                }

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
