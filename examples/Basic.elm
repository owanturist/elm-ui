module Main exposing (main)

{-| -}

import Element exposing (col, el, empty, row, text)


main =
    spacing row
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
            -- , Element.alignX Element.center -- wrap s
            -- , Element.centerY -- non wrap
            -- , Element.alignBottom -- non wrap
            -- , Element.alignX Element.right -- wrap u
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
            -- , Element.alignY Element.center -- wrap s
            -- , Element.alignY Element.bottom -- wrap u
            -- , Element.alignRight -- non wrap
            , Element.align Element.center Element.center
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


alignDocs =
    row
        [ Element.background (Element.rgb 100 100 200)
        , Element.width Element.fill
        ]
        [ el [ Element.background (Element.rgb 100 100 100) ] (text "1")
        , el [ Element.background (Element.rgb 100 100 100), Element.alignX Element.left ] (text "2")
        , el [ Element.background (Element.rgb 100 100 100), Element.alignX Element.center ] (text "3")
        , el [ Element.background (Element.rgb 100 100 100), Element.alignX Element.right ] (text "4")
        ]


spacing node =
    List.range 1 5
        |> List.map
            (el
                [ Element.background (Element.rgb 200 200 200)
                , Element.padding 10
                ]
                << text
                << String.fromInt
            )
        |> node
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 0
            , Element.background (Element.rgb 100 100 100)
            , Element.alpha 0.5
            , Element.wordSpacing 0.5
            ]
