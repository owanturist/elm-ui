module Main exposing (main)

{-| -}

import Element exposing (col, el, row, text)


main =
    col
        []
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
        |> Element.layout
            []
