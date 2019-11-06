module Main exposing (main)

{-| -}

import Browser
import Element exposing (col, el, empty, image, row, text)
import Task
import Time


type Msg
    = Tick Time.Posix


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 0
    , Time.now
        |> Task.perform Tick
    )


type alias Model =
    { start : Int
    , now : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update (Tick now) model =
    if model.start == 0 then
        ( Model (Time.posixToMillis now) (Time.posixToMillis now)
        , Cmd.none
        )

    else
        ( { model | now = Time.posixToMillis now }
        , Cmd.none
        )


subscriptions _ =
    Time.every 16 Tick


src =
    "https://images2.minutemediacdn.com/image/upload/c_crop,h_1193,w_2121,x_0,y_64/f_auto,q_auto,w_1100/v1565279671/shape/mentalfloss/578211-gettyimages-542930526.jpg"


duration =
    10000


view model =
    List.range 1 60
        |> List.map
            (\i ->
                let
                    s =
                        model.now - i * 100

                    t =
                        if s > model.start then
                            modBy duration (s - model.start)

                        else
                            0

                    angle =
                        if t == 0 then
                            0

                        else
                            toFloat t / duration * 359
                in
                el
                    [ Element.background (Element.rgb 80 80 80)
                    , Element.height (Element.px 125)
                    , Element.width (Element.px 1)
                    , Element.rotate angle
                    , Element.alignX Element.center
                    ]
                    empty
                    |> el [ Element.width (Element.px 150) ]
            )
        |> row
            [ Element.spacing Element.evenly
            , Element.wrapped Element.evenly
            , Element.padding 25
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.background (Element.rgb 40 40 40)
            ]
        |> Element.layout []


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
