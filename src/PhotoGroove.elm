module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)



--MAIN


main =
    view "no model yet"



--VIEW


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumb-nails" ]
            [ img [ src "https://elm-in-action.com/1.jpeg" ] []
            , img [ src "https://elm-in-action.com/2.jpeg" ] []
            , img [ src "https://elm-in-action.com/3.jpeg" ] []
            ]
        ]
