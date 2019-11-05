module Main exposing (main)

{-| -}

import Element exposing (column, download, downloadAs, el, image, link, newTabLink, none, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


main =
    imageEl
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
            , Element.centerX -- non wrap
            , Element.centerY -- wrap s.s.e.ctr.accy

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


alignDocs =
    row
        [ Background.color (Element.rgb255 100 100 200)
        , Element.width Element.fill
        ]
        [ el [ Background.color (Element.rgb255 100 100 100) ] (text "1")
        , el [ Background.color (Element.rgb255 100 100 100), Element.alignLeft ] (text "2")
        , el [ Background.color (Element.rgb255 100 100 100), Element.centerX ] (text "3")
        , el [ Background.color (Element.rgb255 100 100 100), Element.alignRight ] (text "4")
        ]


spacing node =
    List.range 1 5
        |> List.map
            (el
                [ Background.color (Element.rgb255 200 200 200)
                , Element.padding 10
                , Element.pointer
                ]
                << text
                << String.fromInt
            )
        |> node
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 20
            , Background.color (Element.rgb255 100 100 100)
            ]


wrappedRowWrap =
    column
        [ Element.spacing 30
        , Element.padding 60
        , Element.height (Element.px 500)
        , Element.centerX
        , Element.centerY
        , Background.color (Element.rgb255 200 150 150)
        ]
        [ el
            [ Element.width Element.fill
            , Background.color (Element.rgb255 150 150 150)
            ]
            (text "hi")
        , List.range 0 10
            |> List.map
                (el
                    [ Element.width (Element.px 100)
                    , Background.color (Element.rgb255 200 200 200)
                    ]
                    << text
                    << String.fromInt
                )
            |> wrappedRow
                [ Background.color (Element.rgb255 100 100 100)
                , Element.spaceEvenly
                , Element.height Element.fill
                ]
        , el
            [ Element.width Element.fill
            , Background.color (Element.rgb255 150 150 150)
            ]
            (text "by")
        ]


moveEl =
    el
        [ Element.width (Element.px 50)
        , Element.height (Element.px 100)
        , Element.moveRight 100
        , Element.scale 2
        , Element.centerX
        , Element.centerY
        , Element.moveDown 100
        , Background.color (Element.rgb255 150 150 150)
        , Element.explain Debug.todo
        ]
        (text "hi")


clipEl =
    el
        [ Element.width (Element.px 150)
        , Element.height (Element.px 300)
        ]
        none
        |> el
            [ Element.explain Debug.todo
            , Element.width (Element.px 100)
            , Element.height (Element.px 200)

            -- , Element.scrollbars
            , Element.clip
            ]


linkEl =
    column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ downloadAs
            [ Element.width (Element.px 200)
            , Element.height (Element.px 100)
            , Element.explain Debug.todo
            , Element.alignBottom
            ]
            { url = "/"
            , filename = "tests"
            , label =
                row []
                    [ text "link"
                    , text "text"
                    ]
            }
        ]


cat =
    "https://images2.minutemediacdn.com/image/upload/c_crop,h_1193,w_2121,x_0,y_64/f_auto,q_auto,w_1100/v1565279671/shape/mentalfloss/578211-gettyimages-542930526.jpg"


imageEl =
    image
        [ Background.color (Element.rgb 0.5 0.5 0.5)
        , Element.width (Element.px 200)
        ]
        { description = "hola"
        , src = cat
        }
        |> List.repeat 10
        |> wrappedRow
            [ Element.width Element.fill
            , Element.spaceEvenly
            ]
