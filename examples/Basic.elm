module Main exposing (main)

{-| -}

import Element exposing (col, el, empty, image, row, text)


main =
    imageEl
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
                , Element.pointer
                ]
                << text
                << String.fromInt
            )
        |> node
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing (Element.px 20)
            , Element.background (Element.rgb 100 100 100)
            ]


wrappedRowWrap =
    col
        [ Element.spacing (Element.px 30)
        , Element.padding 60
        , Element.height (Element.px 500)
        , Element.align Element.center Element.center
        , Element.background (Element.rgb 200 150 150)
        ]
        [ el
            [ Element.width Element.fill
            , Element.background (Element.rgb 150 150 150)
            ]
            (text "hi")
        , List.range 0 3
            |> List.map
                (el
                    [ Element.width (Element.px 100)
                    , Element.background (Element.rgb 200 200 200)
                    ]
                    << text
                    << String.fromInt
                )
            |> row
                [ Element.background (Element.rgb 100 100 100)
                , Element.wrapped (Element.px 10)
                , Element.spacing (Element.px 50)
                , Element.width (Element.px 500)

                -- , Element.height Element.fill
                ]
        , el
            [ Element.width Element.fill
            , Element.background (Element.rgb 150 150 150)
            ]
            (text "by")
        ]


moveEl =
    el
        [ Element.width (Element.px 50)
        , Element.height (Element.px 100)
        , Element.rotate 45
        , Element.scale 2
        , Element.background (Element.rgb 150 150 150)
        , Element.explain Debug.todo
        , Element.align Element.center Element.center
        ]
        (text "hi")


clipEl =
    el
        [ Element.width (Element.px 150)
        , Element.height (Element.px 300)
        ]
        empty
        |> el
            [ Element.explain Debug.todo
            , Element.width (Element.px 100)
            , Element.height (Element.px 200)
            , Element.clipX
            , Element.scrollY
            ]


linkEl =
    col
        [ Element.width (Element.px 200)
        , Element.height (Element.px 100)
        , Element.explain Debug.todo
        , Element.link "./Rotation.elm"
        , Element.alignY Element.bottom
        ]
        [ text "hi"
        ]


cat =
    "https://images2.minutemediacdn.com/image/upload/c_crop,h_1193,w_2121,x_0,y_64/f_auto,q_auto,w_1100/v1565279671/shape/mentalfloss/578211-gettyimages-542930526.jpg"


imageEl =
    image
        [ Element.background (Element.rgb 100 100 100)
        , Element.width (Element.px 200)
        , Element.download "/"
        ]
        { alt = "hola"
        , src = cat
        }
        |> List.repeat (8 * 8)
        |> row
            [ Element.spacing Element.evenly
            , Element.wrapped (Element.px 10)
            ]
