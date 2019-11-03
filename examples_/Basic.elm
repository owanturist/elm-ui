module Main exposing (main)

{-| -}

import Element exposing (column, el, none, row, text)
import Element.Background as Background
import Element.Font as Font


main =
    alignSingleInRow
        |> Element.layout []


alignSingleInRow =
    row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ el
            [ Background.color (Element.rgb255 100 100 200)
            , Element.width (Element.px 500)
            , Element.height (Element.px 500)

            -- , Element.alignLeft -- non wrap
            -- , Element.alignTop -- non wrap
            -- , Element.centerX -- wrap s.s.e.ctr.ccy.accx
            -- , Element.centerY -- non wrap
            -- , Element.alignBottom -- non wrap
            -- , Element.alignRight -- wrap u.s.e.ctr.ccy.acr
            ]
            none
        ]


alignSingleInCol =
    column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ el
            [ Background.color (Element.rgb255 100 100 200)
            , Element.width (Element.px 500)
            , Element.height (Element.px 500)

            -- , Element.alignLeft -- non wrap
            -- , Element.alignTop -- non wrap
            -- , Element.centerX -- non wrap
            -- , Element.centerY -- wrap s.s.e.ctr.accy
            -- , Element.alignBottom -- wrap u.s.e.ctr.acb
            -- , Element.alignRight -- non wrap
            ]
            none
        ]


alignSingle =
    el
        [ Background.color (Element.rgb255 100 100 200)
        , Element.width (Element.px 500)
        , Element.height (Element.px 500)

        -- , Element.alignLeft -- non wrap
        -- , Element.alignTop -- non wrap
        -- , Element.centerX -- non wrap
        -- , Element.centerY -- non wrap
        -- , Element.alignBottom -- non wrap
        -- , Element.alignRight -- non wrap
        ]
        none
