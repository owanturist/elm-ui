module Main exposing (main)

{-| -}

import Element exposing (col, el, row, text)


main =
    col
        []
        [ text "1"
        , el [] (text "2")
        , row
            [ Element.padding 20 20 20 20
            ]
            [ text "3"
            , text "4"
            ]
        ]
        |> Element.layout
            [ Element.padding 20 20 20 20
            ]
