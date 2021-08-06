module PhotoFolders exposing (main)

import Array
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
    | ClickedFolder FolderPath
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
        , expanded : Bool
        , photoUrls : List String
        , subFolders : List Folder
        }


type FolderPath
    = ThisOne
    | FolderInside Int FolderPath


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com"


{-| given a path to a folder that we want to expand,
recursively descend until we reach the end of the path
and toggle that folder's expanded value
-}
toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        ThisOne ->
            Folder { folder | expanded = not folder.expanded }

        FolderInside nextIndex remainingPath ->
            let
                dummyFolder = Folder {name = "", expanded = False, photoUrls = [], subFolders = []}

                subFolderArray =
                    Array.fromList folder.subFolders

                folderToToggle =
                    Array.get nextIndex subFolderArray
                    |> Maybe.withDefault dummyFolder

                updatedFolder =
                    toggleExpanded remainingPath folderToToggle

                updatedSubfolders : List Folder
                updatedSubfolders =
                    Array.set nextIndex updatedFolder subFolderArray
                    |> Array.toList
            in
            Folder
                { folder | subFolders = updatedSubfolders }


view : Model -> Html Msg
view model =
    let
        photoAt url =
            Dict.get url model.photos
    in
    div [ class "content" ]
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder ThisOne model.root
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


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        ThisOne ->
            FolderInside index ThisOne

        FolderInside nextIndex remainingPath ->
            FolderInside nextIndex (appendIndex index remainingPath)


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photos"
        , onClick (ClickedPhoto url)
        , src (urlPrefix ++ "/photos/" ++ url ++ "/thumb")
        ]
        []


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        folderLabel =
            label [ onClick (ClickedFolder path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            viewSubfolder : Int -> Folder -> Html Msg
            viewSubfolder index subFolder =
                viewFolder (appendIndex index path) subFolder
        in
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "subfolders" ] (List.indexedMap viewSubfolder folder.subFolders)
            ]

    else
        div [ class "folder collapsed" ]
            [ folderLabel ]


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

        ClickedFolder path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none )


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        initialModel : Model
        initialModel =
            { selectedPhotoUrl = Nothing
            , photos = Dict.empty
            , root =
                Folder
                    { name = "empty"
                    , expanded = False
                    , photoUrls = []
                    , subFolders = []
                    }
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
                        , expanded = True
                        , photoUrls = []
                        , subFolders =
                            [ Folder
                                { name = "outdoors"
                                , expanded = True
                                , photoUrls = []
                                , subFolders = []
                                }
                            , Folder
                                { name = "indoors"
                                , expanded = True
                                , photoUrls = [ "fresco" ]
                                , subFolders = []
                                }
                            , Folder
                                { name = "2017"
                                , expanded = True
                                , photoUrls = []
                                , subFolders =
                                    [ Folder
                                        { name = "outdoors"
                                        , expanded = True
                                        , photoUrls = []
                                        , subFolders = []
                                        }
                                    , Folder
                                        { name = "indoors"
                                        , expanded = True
                                        , photoUrls = []
                                        , subFolders = []
                                        }
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
