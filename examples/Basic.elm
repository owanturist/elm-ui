module Main exposing (main)

{-| -}

import Element exposing (col, el, empty, row, text)


main =
    alignSingleInRow
        |> Element.layout []


alignSingleInRow =
    row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ el
            [ Element.background (Element.rgb 100 100 200)
            , Element.width (Element.px 500)
            , Element.height (Element.px 500)

            -- , Element.alignLeft -- non wrap
            -- , Element.alignTop -- non wrap
            , Element.alignX Element.center -- wrap s

            -- , Element.centerY -- non wrap
            -- , Element.alignBottom -- non wrap
            -- , Element.alignRight -- wrap u
            ]
            empty
        ]


alignSingleInCol =
    col
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ el
            [ Element.background (Element.rgb 100 100 200)
            , Element.width (Element.px 500)
            , Element.height (Element.px 500)

            -- , Element.alignLeft -- non wrap
            -- , Element.alignTop -- non wrap
            -- , Element.centerX -- non wrap
            -- , Element.centerY -- wrap s
            -- , Element.alignBottom -- wrap u
            -- , Element.alignRight -- non wrap
            ]
            empty
        ]


alignSingle =
    el
        [ Element.background (Element.rgb 100 100 200)
        , Element.width (Element.px 500)
        , Element.height (Element.px 500)

        -- , Element.alignLeft -- non wrap
        -- , Element.alignTop -- non wrap
        -- , Element.centerX -- non wrap
        -- , Element.centerY -- non wrap
        -- , Element.alignBottom -- non wrap
        -- , Element.alignRight -- non wrap
        ]
        empty
