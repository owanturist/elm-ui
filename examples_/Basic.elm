module Main exposing (main)

{-| -}

import Element exposing (column, el, none, row, text)
import Element.Background as Background
import Element.Font as Font


main =
    column
        [ Font.justify
        ]
        [ text "1"
        , el
            []
            (text "2")
        , row
            [ Font.wordSpacing 50.5
            ]
            [ text "34"
            , text " 56 78"
            ]
        ]
        |> Element.layout
            []
