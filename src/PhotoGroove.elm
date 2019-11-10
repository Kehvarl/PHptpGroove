module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)



--MAIN


main =
    view initialModel



--MODEL


initialModel =
    [ { url = "1.jpeg" }
    , { url = "2.jpeg" }
    , { url = "3.jpeg" }
    ]



--VIEW


urlPrefix =
    "https://elm-in-action.com/"


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumb-nails" ]
            (List.map viewThumbnail model)
        ]


viewThumbnail thumb =
    img [ src (urlPrefix ++ thumb.url) ] []
