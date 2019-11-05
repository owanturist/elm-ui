module Main exposing (main)

{-| -}

import Browser
import Element exposing (col, el, empty, row, text)
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


view model =
    let
        duration =
            1000

        t =
            modBy duration (model.now - model.start)

        angle =
            if t == 0 then
                0

            else
                toFloat t / duration * 359
    in
    el
        [ Element.width (Element.px 50)
        , Element.height (Element.px 100)
        , Element.rotate angle
        , Element.background (Element.rgb 150 150 150)
        , Element.explain Debug.todo
        , Element.align Element.center Element.center
        ]
        (text "ok")
        |> Element.layout []


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
