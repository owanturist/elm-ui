module Main exposing (main)

{-| -}

import Element exposing (column, el, none, row, text)
import Element.Background as Background
import Element.Font as Font


main =
    row
        [ Background.color (Element.rgb255 100 100 200)
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ el
            [ Element.width (Element.fillPortion 1)
            , Element.height (Element.fillPortion 2)
            ]
            (text "1")
        , el
            [ Element.width (Element.fillPortion 1)
            , Element.height (Element.fillPortion 2)
            ]
            (text "2")
        , el
            [ Element.width (Element.fillPortion 2)
            , Element.height (Element.fillPortion 1)
            ]
            (text "3")
        ]
        |> Element.layout []
