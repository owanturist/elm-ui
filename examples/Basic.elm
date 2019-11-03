module Main exposing (main)

{-| -}

import Element exposing (col, el, row, text)


main =
    col
        [ Element.background (Element.rgb 100 100 200)
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ el
            [ Element.height (Element.maximum 200 (Element.portion 2))
            , Element.background (Element.rgb 100 200 100)
            ]
            (text "1")
        , el
            [ Element.width (Element.portion 2)
            , Element.background (Element.rgb 200 200 100)
            ]
            (text "2")
        , el
            [ Element.width (Element.portion 1)
            , Element.background (Element.rgb 100 200 200)
            ]
            (text "3")
        ]
        |> Element.layout []
