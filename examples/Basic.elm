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
            []
            [ text "3"
            , text "4"
            ]
        ]
        |> Element.layout []
