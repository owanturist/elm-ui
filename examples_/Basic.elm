module Main exposing (main)

{-| -}

import Element exposing (column, el, none, row, text)
import Element.Background as Background
import Element.Font as Font


main =
    column
        [ Background.color (Element.rgb255 100 100 200)
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ el
            [ Element.height (Element.maximum 200 (Element.fillPortion 2))
            , Background.color (Element.rgb255 100 200 100)
            ]
            (text "1")
        , el
            [ Element.width (Element.fillPortion 2)
            , Background.color (Element.rgb255 200 200 100)
            ]
            (text "2")
        , el
            [ Element.width (Element.fillPortion 1)
            , Background.color (Element.rgb255 100 200 200)
            ]
            (text "3")
        ]
        |> Element.layout []
