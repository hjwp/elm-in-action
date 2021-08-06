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
    | ClickedFolder FolderLocator
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


type FolderLocator
    = ThisOne
    | FolderInside Int FolderLocator


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com"


{-| given a locator for a folder that we want to expand,
recursively descend until we reach the end of the locator
and toggle that folder's expanded value
-}
toggleExpanded : FolderLocator -> Folder -> Folder
toggleExpanded locator (Folder folder) =
    case locator of
        ThisOne ->
            Folder { folder | expanded = not folder.expanded }

        FolderInside nextIndex remainingPath ->
            let
                toggledSubfolders =
                    folder.subFolders
                        |> replaceItemAt
                            nextIndex
                            (toggleExpanded remainingPath)
            in
            Folder
                { folder | subFolders = toggledSubfolders }


{-| "replaces" an item at a given index in a list
by applying a function to it
(returns a new list obvs. other elements untouched)
-}
replaceItemAt : Int -> (a -> a) -> List a -> List a
replaceItemAt index f list =
    let
        replaceIfIth i item =
            if i == index then
                f item

            else
                item
    in
    List.indexedMap replaceIfIth list


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


appendIndex : Int -> FolderLocator -> FolderLocator
appendIndex index locator =
    case locator of
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


viewFolder : FolderLocator -> Folder -> Html Msg
viewFolder locator (Folder folder) =
    let
        folderLabel =
            label [ onClick (ClickedFolder locator) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            viewSubfolder : Int -> Folder -> Html Msg
            viewSubfolder index subFolder =
                viewFolder (appendIndex index locator) subFolder
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

        ClickedFolder locator ->
            ( { model | root = toggleExpanded locator model.root }, Cmd.none )


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        initialModel : Model
        initialModel =
            { selectedPhotoUrl = Nothing
            , photos = Dict.empty
            , root =
                Folder
                    { name = "Loading folders..."
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
