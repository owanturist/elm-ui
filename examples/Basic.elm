module Main exposing (main)

{-| -}

import Element exposing (col, el, row, text)


main =
    col
        [ Element.fontAlign Element.justify
        ]
        [ text "1"
        , el
            []
            (text "2")
        , row
            [ Element.wordSpacing 50.5
            ]
            [ text "34"
            , text " 56 78"
            ]
        ]
        |> Element.layout []
