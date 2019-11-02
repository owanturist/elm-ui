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
            [ Element.padding 20 20 20 20
            , Element.padding 30 30 30 30
            , Element.background (Element.rgb 100 100 200)
            ]
