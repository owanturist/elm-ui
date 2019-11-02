module Main exposing (main)

{-| -}

import Element exposing (column, el, row, text)
import Element.Background as Background


main =
    column
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
            [ Element.padding 20
            , Element.padding 30
            , Background.color (Element.rgb255 100 100 200)
            ]
