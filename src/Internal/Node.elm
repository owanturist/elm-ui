module Internal.Node exposing
    ( Alignment(..)
    , Font(..)
    , Layout(..)
    , Length(..)
    , Node(..)
    , Prop(..)
    , render
    )

import Dict exposing (Dict)
import Internal.Box as Box exposing (Box)
import Internal.Color as Color exposing (Color)
import Internal.Static as Static
import Json.Encode as Encode
import Murmur3
import VirtualDom


type Node msg
    = Empty
    | Text String
    | Element Layout (List (Prop msg)) (List (Node msg))


type Layout
    = Single
    | Row
    | Col


type Font
    = Serif
    | SansSerif
    | Monospace
    | TypeFace String


type Prop msg
    = Attribute (VirtualDom.Attribute msg)
    | Batch (List (Prop msg))
      -- G E O M E T R Y
    | Spacing Int
    | Padding (Box Int)
    | Width Length
    | Height Length
    | AlignX Alignment
    | AlignY Alignment
      -- D R E S S
    | Background Color
    | Opacity Float
      -- F O N T S
    | FontColor Color
    | FontSize Int
    | FontFamily (List Font)
    | FontAlign Alignment
    | FontDecoration String
    | LetterSpacing Float
    | WordSpacing Float


type Length
    = Shrink
    | Portion Int
    | Px Int
    | Minimum Int Length
    | Maximum Int Length


type Alignment
    = Start
    | Middle
    | End
    | Justify


type alias Context =
    { spacings : Dict String String
    , paddings : Dict String String
    , widths : Dict String String
    , heights : Dict String String

    -- D R E S S
    , backgrounds : Dict String String
    , opacities : Dict String String

    -- F O N T S
    , fontColors : Dict String String
    , fontSizes : Dict String String
    , fontFamilies : Dict String String
    , letterSpacings : Dict String String
    , wordSpacings : Dict String String
    }


initialContext : Context
initialContext =
    { spacings = Dict.empty
    , paddings = Dict.empty
    , widths = Dict.empty
    , heights = Dict.empty

    -- D R E S S
    , backgrounds = Dict.empty
    , opacities = Dict.empty

    -- F O N T S
    , fontColors = Dict.empty
    , fontSizes = Dict.empty
    , fontFamilies = Dict.empty
    , letterSpacings = Dict.empty
    , wordSpacings = Dict.empty
    }


type alias Config msg =
    { attributes : List (VirtualDom.Attribute msg)

    -- G E O M E T R Y
    , spacing : Maybe Int
    , padding : Maybe (Box Int)
    , width : Maybe Length
    , height : Maybe Length
    , alignX : Maybe Alignment
    , alignY : Maybe Alignment

    -- D R E S S
    , background : Maybe Color
    , opacity : Float

    -- F O N T S
    , fontColor : Maybe Color
    , fontSize : Maybe Int
    , fontFamily : Maybe (List Font)
    , fontAlign : Maybe Alignment
    , fontDecoration : Maybe String
    , letterSpacing : Maybe Float
    , wordSpacing : Maybe Float
    }


emptyConfig : Config msg
emptyConfig =
    { attributes = []

    -- G E O M E T R Y
    , spacing = Nothing
    , padding = Nothing
    , width = Nothing
    , height = Nothing
    , alignX = Nothing
    , alignY = Nothing

    -- D R E S S
    , background = Nothing
    , opacity = 1

    -- F O N T S
    , fontColor = Nothing
    , fontSize = Nothing
    , fontFamily = Nothing
    , fontAlign = Nothing
    , fontDecoration = Nothing
    , letterSpacing = Nothing
    , wordSpacing = Nothing
    }


initialUiConfig : Config msg
initialUiConfig =
    { emptyConfig
        | background = Just (Color.Rgba 255 255 255 1)
        , fontColor = Just (Color.Rgba 0 0 0 1)
        , fontSize = Just 20
        , fontFamily = Just [ TypeFace "Open Sans", TypeFace "Helvetica", TypeFace "Verdana", SansSerif ]
    }


initialElementConfig : Config msg
initialElementConfig =
    { emptyConfig
        | width = Just Shrink
        , height = Just Shrink
    }


hash : String -> Int
hash =
    Murmur3.hashString 0


applyPropToConfig : Prop msg -> Config msg -> Config msg
applyPropToConfig prop config =
    case prop of
        Attribute attr ->
            Debug.todo "Attribute"

        Batch props ->
            List.foldr applyPropToConfig config props

        -- G E O M E T R Y
        Spacing space ->
            { config | spacing = Just space }

        Padding box ->
            { config | padding = Just box }

        Width width ->
            { config | width = Just width }

        Height height ->
            { config | height = Just height }

        AlignX alignment ->
            { config | alignX = Just alignment }

        AlignY alignment ->
            { config | alignY = Just alignment }

        -- D R E S S
        Background color ->
            { config | background = Just color }

        -- D R E S S
        Opacity x ->
            { config | opacity = x }

        -- F O N T S
        --
        FontColor color ->
            { config | fontColor = Just color }

        FontSize size ->
            { config | fontSize = Just size }

        FontFamily family ->
            { config | fontFamily = Just family }

        FontAlign align ->
            { config | fontAlign = Just align }

        FontDecoration decoration ->
            { config | fontDecoration = Just decoration }

        LetterSpacing spacing ->
            { config | letterSpacing = Just spacing }

        WordSpacing spacing ->
            { config | wordSpacing = Just spacing }


applyPropsToConfig : List (Prop msg) -> Config msg -> Config msg
applyPropsToConfig props config =
    List.foldr applyPropToConfig config props


type alias Acc msg =
    ( Context, List (VirtualDom.Attribute msg) )


applySpacing : Layout -> Maybe Int -> Acc msg -> Acc msg
applySpacing layout spacing ( context, attributes ) =
    case spacing of
        Nothing ->
            ( context
            , if layout == Single then
                attributes

              else
                class "sev" :: attributes
            )

        Just space ->
            let
                className =
                    "s-" ++ String.fromInt space

                ( selector, css ) =
                    case layout of
                        Single ->
                            ( ".empty", "" )

                        Row ->
                            ( ".r." ++ className ++ ">.s+.s"
                            , "margin-left:" ++ px space ++ ";"
                            )

                        Col ->
                            ( ".c." ++ className ++ ">.s+.s"
                            , "margin-top:" ++ px space ++ ";"
                            )
            in
            ( { context | paddings = Dict.insert selector css context.paddings }
            , class className :: attributes
            )


applyPadding : Box Int -> Acc msg -> Acc msg
applyPadding padding ( context, attributes ) =
    let
        className =
            Box.toClass "p" String.fromInt padding

        css =
            Box.toCss "padding" px padding
    in
    ( { context | paddings = Dict.insert ("." ++ className) css context.paddings }
    , class className :: attributes
    )


applyWidth : Length -> Acc msg -> Acc msg
applyWidth length ( context, attributes ) =
    case length of
        Shrink ->
            ( context
            , class "wc" :: attributes
            )

        Portion 1 ->
            ( context
            , class "wf" :: attributes
            )

        Portion n ->
            let
                className =
                    "wp-" ++ String.fromInt n

                css =
                    "flex-grow:" ++ String.fromInt n ++ "00000;"
            in
            ( { context | widths = Dict.insert (".r>." ++ className) css context.widths }
            , class ("wfp " ++ className) :: attributes
            )

        Px x ->
            let
                className =
                    "wpx-" ++ String.fromInt x

                css =
                    "width:" ++ px x ++ ";"
            in
            ( { context | widths = Dict.insert ("." ++ className) css context.widths }
            , class ("we " ++ className) :: attributes
            )

        Minimum x subLength ->
            let
                className =
                    "wmn-" ++ String.fromInt x

                css =
                    "min-width:" ++ px x ++ ";"
            in
            applyWidth subLength
                ( { context | widths = Dict.insert ("." ++ className) css context.widths }
                , class className :: attributes
                )

        Maximum x subLength ->
            let
                className =
                    "wmx-" ++ String.fromInt x

                css =
                    "max-width:" ++ px x ++ ";"
            in
            applyWidth subLength
                ( { context | widths = Dict.insert ("." ++ className) css context.widths }
                , class className :: attributes
                )


applyHeight : Length -> Acc msg -> Acc msg
applyHeight length ( context, attributes ) =
    case length of
        Shrink ->
            ( context
            , class "hc" :: attributes
            )

        Portion 1 ->
            ( context
            , class "hf" :: attributes
            )

        Portion n ->
            let
                className =
                    "hp-" ++ String.fromInt n

                css =
                    "flex-grow:" ++ String.fromInt n ++ "00000;"
            in
            ( { context | heights = Dict.insert (".c>." ++ className) css context.heights }
            , class ("hfp " ++ className) :: attributes
            )

        Px x ->
            let
                className =
                    "hpx-" ++ String.fromInt x

                css =
                    "height:" ++ px x ++ ";"
            in
            ( { context | heights = Dict.insert ("." ++ className) css context.heights }
            , class ("he " ++ className) :: attributes
            )

        Minimum x subLength ->
            let
                className =
                    "hmn-" ++ String.fromInt x

                css =
                    "min-height:" ++ px x ++ ";"
            in
            applyHeight subLength
                ( { context | widths = Dict.insert ("." ++ className) css context.widths }
                , class className :: attributes
                )

        Maximum x subLength ->
            let
                className =
                    "hmx-" ++ String.fromInt x

                css =
                    "max-height:" ++ px x ++ ";"
            in
            applyHeight subLength
                ( { context | widths = Dict.insert ("." ++ className) css context.widths }
                , class className :: attributes
                )


applyAlignX : Alignment -> Acc msg -> Acc msg
applyAlignX alignment ( context, attributes ) =
    ( context
    , case alignment of
        Start ->
            class "ah al" :: attributes

        Middle ->
            class "ah cx" :: attributes

        End ->
            class "ah ar" :: attributes

        Justify ->
            attributes
    )


applyAlignY : Alignment -> Acc msg -> Acc msg
applyAlignY alignment ( context, attributes ) =
    ( context
    , case alignment of
        Start ->
            class "av at" :: attributes

        Middle ->
            class "av cy" :: attributes

        End ->
            class "av ab" :: attributes

        Justify ->
            attributes
    )


applyBackground : Color -> Acc msg -> Acc msg
applyBackground background ( context, attributes ) =
    let
        className =
            Color.toClass "bg" background

        css =
            Color.toCss "background-color" background
    in
    ( { context | backgrounds = Dict.insert ("." ++ className) css context.backgrounds }
    , class className :: attributes
    )


applyOpacity : Float -> Acc msg -> Acc msg
applyOpacity opacity ( context, attributes ) =
    if opacity == 1 then
        ( context, attributes )

    else
        let
            className =
                "o-" ++ String.replace "." "" (String.fromFloat opacity)

            css =
                "opacity:" ++ String.fromFloat opacity ++ ";"
        in
        ( { context | backgrounds = Dict.insert ("." ++ className) css context.backgrounds }
        , class className :: attributes
        )


applyFontColor : Color -> Acc msg -> Acc msg
applyFontColor color ( context, attributes ) =
    let
        className =
            Color.toClass "fc" color
    in
    ( { context | fontColors = Dict.insert ("." ++ className) (Color.toCss "color" color) context.fontColors }
    , class className :: attributes
    )


applyFontSize : Int -> Acc msg -> Acc msg
applyFontSize size ( context, attributes ) =
    let
        className =
            "fs-" ++ String.fromInt size

        css =
            "font-size:" ++ px size ++ ";"
    in
    ( { context | fontSizes = Dict.insert ("." ++ className) css context.fontSizes }
    , class className :: attributes
    )


applyFont : Font -> List String -> List String
applyFont font names =
    case font of
        Serif ->
            "serif" :: names

        SansSerif ->
            "sans-serif" :: names

        Monospace ->
            "monospace" :: names

        TypeFace name ->
            quotes name :: names


applyFontFamily : List Font -> Acc msg -> Acc msg
applyFontFamily family ( context, attributes ) =
    let
        names =
            List.foldr applyFont [] family

        className =
            names
                |> List.map String.toLower
                |> String.concat
                |> hash
                |> String.fromInt
                |> (++) "ff-"

        css =
            "font-family:" ++ String.join "," names ++ ";font-variant:normal;"
    in
    ( { context | fontFamilies = Dict.insert ("." ++ className) css context.fontFamilies }
    , class className :: attributes
    )


applyFontAlign : Alignment -> Acc msg -> Acc msg
applyFontAlign alignment ( context, attributes ) =
    ( context
    , case alignment of
        Start ->
            class "tl" :: attributes

        Middle ->
            class "tc" :: attributes

        End ->
            class "tr" :: attributes

        Justify ->
            class "tj" :: attributes
    )


applyFontDecoration : String -> Acc msg -> Acc msg
applyFontDecoration decoration ( context, attributes ) =
    ( context
    , class ("fd-" ++ decoration) :: attributes
    )


applyLetterSpacing : Float -> Acc msg -> Acc msg
applyLetterSpacing spacing ( context, attributes ) =
    let
        spacing_ =
            String.fromFloat spacing

        className =
            "ls-" ++ String.replace "." "" spacing_

        css =
            "letter-spacing:" ++ spacing_ ++ "px;"
    in
    ( { context | letterSpacings = Dict.insert ("." ++ className) css context.letterSpacings }
    , class className :: attributes
    )


applyWordSpacing : Float -> Acc msg -> Acc msg
applyWordSpacing spacing ( context, attributes ) =
    let
        spacing_ =
            String.fromFloat spacing

        className =
            "ws-" ++ String.replace "." "" spacing_

        css =
            "word-spacing:" ++ spacing_ ++ "px;"
    in
    ( { context | wordSpacings = Dict.insert ("." ++ className) css context.wordSpacings }
    , class className :: attributes
    )


applyConfigToContextFn : Maybe (Acc msg -> Acc msg) -> Acc msg -> Acc msg
applyConfigToContextFn fn acc =
    fn
        |> Maybe.map ((|>) acc)
        |> Maybe.withDefault acc


applyConfigToContext : Layout -> Config msg -> Context -> Acc msg
applyConfigToContext layout config context =
    [ Just (applySpacing layout config.spacing)
    , Maybe.map applyPadding config.padding
    , Maybe.map applyWidth config.width
    , Maybe.map applyHeight config.height
    , Maybe.map applyAlignX config.alignX
    , Maybe.map applyAlignY config.alignY
    , Maybe.map applyBackground config.background
    , Just (applyOpacity config.opacity)
    , Maybe.map applyFontColor config.fontColor
    , Maybe.map applyFontSize config.fontSize
    , Maybe.map applyFontFamily config.fontFamily
    , Maybe.map applyFontAlign config.fontAlign
    , Maybe.map applyFontDecoration config.fontDecoration
    , Maybe.map applyLetterSpacing config.letterSpacing
    , Maybe.map applyWordSpacing config.wordSpacing
    ]
        |> List.foldr applyConfigToContextFn ( context, config.attributes )


class : String -> VirtualDom.Attribute msg
class className =
    VirtualDom.property "className" (Encode.string className)


empty : VirtualDom.Node msg
empty =
    VirtualDom.text ""


renderEmpty : Context -> ( Context, VirtualDom.Node msg )
renderEmpty context =
    ( context, empty )


renderText : String -> Layout -> Context -> ( Context, VirtualDom.Node msg )
renderText txt parent context =
    ( context
    , VirtualDom.node "div"
        [ if parent == Single then
            class "s t wf hf"

          else
            class "s t wc hc"
        ]
        [ VirtualDom.text txt ]
    )


layoutToClassName : Layout -> String
layoutToClassName layout =
    case layout of
        Single ->
            "s e"

        Row ->
            "s r cl ccy"

        Col ->
            "s c cl ct"


renderAlignContainer : String -> String -> VirtualDom.Node msg -> VirtualDom.Node msg
renderAlignContainer tag className vnode =
    VirtualDom.node tag [ class ("s e ctr" ++ className) ] [ vnode ]


wrapAlignContainer : Layout -> Config msg -> VirtualDom.Node msg -> VirtualDom.Node msg
wrapAlignContainer layout config vnode =
    case ( layout, config.alignX, config.alignY ) of
        ( Row, Just Middle, _ ) ->
            renderAlignContainer "s" "ccy accx" vnode

        ( Row, Just End, _ ) ->
            renderAlignContainer "u" "ccy acr" vnode

        ( Col, _, Just Middle ) ->
            renderAlignContainer "s" "accy" vnode

        ( Col, _, Just End ) ->
            renderAlignContainer "u" "acb" vnode

        _ ->
            vnode


renderElement : Layout -> List (Prop msg) -> List (Node msg) -> Layout -> Context -> ( Context, VirtualDom.Node msg )
renderElement layout props nodes parent context =
    let
        config =
            applyPropsToConfig props initialElementConfig

        ( nextContext, attributes ) =
            applyConfigToContext layout config context

        ( finalContext, children ) =
            List.foldr
                (\node ( contextAcc, childrenAcc ) ->
                    Tuple.mapSecond
                        (\child -> child :: childrenAcc)
                        (renderHelp layout contextAcc node)
                )
                ( nextContext, [] )
                nodes
    in
    ( finalContext
    , VirtualDom.node "div"
        (class (layoutToClassName layout) :: attributes)
        children
        |> wrapAlignContainer parent config
    )


renderHelp : Layout -> Context -> Node msg -> ( Context, VirtualDom.Node msg )
renderHelp parent context node =
    case node of
        Empty ->
            renderEmpty context

        Text txt ->
            renderText txt parent context

        Element layout props nodes ->
            renderElement layout props nodes parent context


px : Int -> String
px x =
    String.fromInt x ++ "px"


wrap : String -> String -> String -> String
wrap pre post x =
    pre ++ x ++ post


quotes : String -> String
quotes =
    wrap "\"" "\""


curlyBraces : String -> String
curlyBraces =
    wrap "{" "}"


renderSelectors : Dict String String -> List ( String, VirtualDom.Node msg ) -> List ( String, VirtualDom.Node msg )
renderSelectors selectors nodes =
    Dict.foldr
        (\selector rules acc -> ( selector, VirtualDom.text (selector ++ curlyBraces rules) ) :: acc)
        nodes
        selectors


renderContext : Context -> VirtualDom.Node msg
renderContext context =
    []
        |> renderSelectors context.paddings
        |> renderSelectors context.widths
        |> renderSelectors context.heights
        |> renderSelectors context.backgrounds
        |> renderSelectors context.fontColors
        |> renderSelectors context.fontSizes
        |> renderSelectors context.fontFamilies
        |> renderSelectors context.letterSpacings
        |> renderSelectors context.wordSpacings
        |> VirtualDom.keyedNode "style" []


staticCss : VirtualDom.Node msg
staticCss =
    VirtualDom.node "style" [] [ VirtualDom.text Static.css ]


render : List (Prop msg) -> Node msg -> VirtualDom.Node msg
render props node =
    let
        config =
            applyPropsToConfig props initialUiConfig

        ( context, attributes ) =
            applyConfigToContext Single config initialContext

        ( finalContext, vnode ) =
            renderHelp Single context node
    in
    VirtualDom.node "div"
        (class "ui s e" :: attributes)
        [ VirtualDom.lazy (always staticCss) ()
        , renderContext finalContext
        , vnode
        ]
