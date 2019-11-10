module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



--MAIN


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



--MODEL


type alias ImgUrl =
    { url : String }


type alias Model =
    { photos : List ImgUrl
    , selectedUrl : String
    }


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }



--UPDATE


type Msg
    = Selected String


update :
    Msg
    -> Model
    -> Model --( Model, Cmd Msg )
update msg model =
    case msg of
        Selected newUrl ->
            { model | selectedUrl = newUrl }



--VIEW


urlPrefix =
    "https://elm-in-action.com/"


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumb-nails" ]
            (List.map (\photo -> viewThumbnail model.selectedUrl photo) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (Selected thumb.url)
        ]
        []
