module Internal.Node exposing
    ( Alignment(..)
    , Box
    , Color
    , Font(..)
    , Layout(..)
    , Length(..)
    , Node(..)
    , Prop(..)
    , render
    )

import Dict exposing (Dict)
import Internal.Css as Css
import Json.Encode as Encode
import VirtualDom


type alias Color =
    { r : Int
    , b : Int
    , g : Int
    , a : Float
    }


type alias Box =
    { t : Int
    , r : Int
    , b : Int
    , l : Int
    }


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
    | Padding Box
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
      -- S P E C I A L   S T A T E
    | Pointer


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
    , padding : Maybe Box
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

    -- S P E C I A L   S T A T E
    , pointer : Bool
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

    -- S P E C I A L   S T A T E
    , pointer = False
    }


initialUiConfig : Config msg
initialUiConfig =
    { emptyConfig
        | background = Just (Color 255 255 255 1)
        , fontColor = Just (Color 0 0 0 1)
        , fontSize = Just 20
        , fontFamily = Just [ TypeFace "Open Sans", TypeFace "Helvetica", TypeFace "Verdana", SansSerif ]
    }


initialElementConfig : Config msg
initialElementConfig =
    { emptyConfig
        | width = Just Shrink
        , height = Just Shrink
    }


class : String -> VirtualDom.Attribute msg
class className =
    VirtualDom.property "className" (Encode.string className)


classes : List String -> VirtualDom.Attribute msg
classes classNames =
    class (String.join " " classNames)


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
        --
        Background color ->
            { config | background = Just color }

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

        -- S P E C I A L   S T A T E
        Pointer ->
            { config | pointer = True }


applyPropsToConfig : List (Prop msg) -> Config msg -> Config msg
applyPropsToConfig props config =
    List.foldr applyPropToConfig config props


type alias Acc msg =
    ( Context, List (VirtualDom.Attribute msg) )


applySpacing : Layout -> Maybe Int -> Acc msg -> Acc msg
applySpacing layout spacing ( context, attributes ) =
    case ( layout, spacing ) of
        ( Row, Just space ) ->
            let
                ( className, css ) =
                    Css.spacingRow space

                selector =
                    Css.dot Css.row ++ Css.dot className ++ ">" ++ Css.dot Css.any ++ "+" ++ Css.dot Css.any
            in
            ( { context | paddings = Dict.insert selector css context.paddings }
            , class className :: attributes
            )

        ( Col, Just space ) ->
            let
                ( className, css ) =
                    Css.spacingCol space

                selector =
                    Css.dot Css.col ++ Css.dot className ++ ">" ++ Css.dot Css.any ++ "+" ++ Css.dot Css.any
            in
            ( { context | paddings = Dict.insert selector css context.paddings }
            , class className :: attributes
            )

        _ ->
            ( context
            , if layout == Single then
                attributes

              else
                class Css.spaceEvenly :: attributes
            )


applyPadding : Box -> Acc msg -> Acc msg
applyPadding { t, r, b, l } ( context, attributes ) =
    let
        ( className, css ) =
            Css.padding t r b l
    in
    ( { context | paddings = Dict.insert (Css.dot className) css context.paddings }
    , class className :: attributes
    )


applyWidth : Length -> Acc msg -> Acc msg
applyWidth length ( context, attributes ) =
    case length of
        Shrink ->
            ( context
            , class Css.widthContent :: attributes
            )

        Portion 1 ->
            ( context
            , class Css.widthFill :: attributes
            )

        Portion n ->
            let
                ( className, css ) =
                    Css.widthPortion n
            in
            ( { context | widths = Dict.insert (Css.dot Css.row ++ ">" ++ Css.dot className) css context.widths }
            , classes [ Css.widthFillPortion, className ] :: attributes
            )

        Px x ->
            let
                ( className, css ) =
                    Css.widthPx x
            in
            ( { context | widths = Dict.insert (Css.dot className) css context.widths }
            , classes [ Css.widthExact, className ] :: attributes
            )

        Minimum x subLength ->
            let
                ( className, css ) =
                    Css.widthMin x
            in
            applyWidth subLength
                ( { context | widths = Dict.insert (Css.dot className) css context.widths }
                , class className :: attributes
                )

        Maximum x subLength ->
            let
                ( className, css ) =
                    Css.widthMax x
            in
            applyWidth subLength
                ( { context | widths = Dict.insert (Css.dot className) css context.widths }
                , class className :: attributes
                )


applyHeight : Length -> Acc msg -> Acc msg
applyHeight length ( context, attributes ) =
    case length of
        Shrink ->
            ( context
            , class Css.heightContent :: attributes
            )

        Portion 1 ->
            ( context
            , class Css.heightFill :: attributes
            )

        Portion n ->
            let
                ( className, css ) =
                    Css.heightPortion n
            in
            ( { context | heights = Dict.insert (Css.dot Css.col ++ ">" ++ Css.dot className) css context.heights }
            , classes [ Css.heightFillPortion, className ] :: attributes
            )

        Px x ->
            let
                ( className, css ) =
                    Css.heightPx x
            in
            ( { context | heights = Dict.insert (Css.dot className) css context.heights }
            , classes [ Css.heightExact, className ] :: attributes
            )

        Minimum x subLength ->
            let
                ( className, css ) =
                    Css.heightMin x
            in
            applyHeight subLength
                ( { context | widths = Dict.insert (Css.dot className) css context.widths }
                , class className :: attributes
                )

        Maximum x subLength ->
            let
                ( className, css ) =
                    Css.heightMax x
            in
            applyHeight subLength
                ( { context | widths = Dict.insert (Css.dot className) css context.widths }
                , class className :: attributes
                )


applyAlignX : Alignment -> Acc msg -> Acc msg
applyAlignX alignment ( context, attributes ) =
    ( context
    , case alignment of
        Start ->
            classes [ Css.alignedHorizontally, Css.alignLeft ] :: attributes

        Middle ->
            classes [ Css.alignedHorizontally, Css.alignCenterX ] :: attributes

        End ->
            classes [ Css.alignedHorizontally, Css.alignRight ] :: attributes

        Justify ->
            attributes
    )


applyAlignY : Alignment -> Acc msg -> Acc msg
applyAlignY alignment ( context, attributes ) =
    ( context
    , case alignment of
        Start ->
            classes [ Css.alignedVertically, Css.alignTop ] :: attributes

        Middle ->
            classes [ Css.alignedVertically, Css.alignCenterY ] :: attributes

        End ->
            classes [ Css.alignedVertically, Css.alignBottom ] :: attributes

        Justify ->
            attributes
    )


applyBackground : Color -> Acc msg -> Acc msg
applyBackground { r, g, b, a } ( context, attributes ) =
    let
        ( className, css ) =
            Css.backgroundColor r g b a
    in
    ( { context | backgrounds = Dict.insert (Css.dot className) css context.backgrounds }
    , class className :: attributes
    )


applyOpacity : Float -> Acc msg -> Acc msg
applyOpacity x ( context, attributes ) =
    if x == 1 then
        ( context, attributes )

    else
        let
            ( className, css ) =
                Css.opacity x
        in
        ( { context | backgrounds = Dict.insert (Css.dot className) css context.backgrounds }
        , class className :: attributes
        )


applyFontColor : Color -> Acc msg -> Acc msg
applyFontColor { r, g, b, a } ( context, attributes ) =
    let
        ( className, css ) =
            Css.fontColor r g b a
    in
    ( { context | fontColors = Dict.insert (Css.dot className) css context.fontColors }
    , class className :: attributes
    )


applyFontSize : Int -> Acc msg -> Acc msg
applyFontSize size ( context, attributes ) =
    let
        ( className, css ) =
            Css.fontSize size
    in
    ( { context | fontSizes = Dict.insert (Css.dot className) css context.fontSizes }
    , class className :: attributes
    )


fontToString : Font -> String
fontToString font =
    case font of
        Serif ->
            "serif"

        SansSerif ->
            "sans-serif"

        Monospace ->
            "monospace"

        TypeFace name ->
            "\"" ++ name ++ "\""


applyFontFamily : List Font -> Acc msg -> Acc msg
applyFontFamily family ( context, attributes ) =
    let
        ( className, css ) =
            Css.fontFamily (List.map fontToString family)
    in
    ( { context | fontFamilies = Dict.insert (Css.dot className) css context.fontFamilies }
    , class className :: attributes
    )


applyFontAlign : Alignment -> Acc msg -> Acc msg
applyFontAlign alignment ( context, attributes ) =
    ( context
    , case alignment of
        Start ->
            class Css.textLeft :: attributes

        Middle ->
            class Css.textCenter :: attributes

        End ->
            class Css.textRight :: attributes

        Justify ->
            class Css.textJustify :: attributes
    )



-- @TODO


applyFontDecoration : String -> Acc msg -> Acc msg
applyFontDecoration decoration ( context, attributes ) =
    ( context
    , class ("fd-" ++ decoration) :: attributes
    )


applyLetterSpacing : Float -> Acc msg -> Acc msg
applyLetterSpacing spacing ( context, attributes ) =
    let
        ( className, css ) =
            Css.letterSpacing spacing
    in
    ( { context | letterSpacings = Dict.insert (Css.dot className) css context.letterSpacings }
    , class className :: attributes
    )


applyWordSpacing : Float -> Acc msg -> Acc msg
applyWordSpacing spacing ( context, attributes ) =
    let
        ( className, css ) =
            Css.wordSpacing spacing
    in
    ( { context | wordSpacings = Dict.insert (Css.dot className) css context.wordSpacings }
    , class className :: attributes
    )


applyPointer : Bool -> Acc msg -> Acc msg
applyPointer x ( context, attributes ) =
    ( context
    , if x then
        class Css.cursorPointer :: attributes

      else
        attributes
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
    , Just (applyPointer config.pointer)
    ]
        |> List.foldr applyConfigToContextFn ( context, config.attributes )


empty : VirtualDom.Node msg
empty =
    VirtualDom.text ""


renderEmpty : Context -> ( Context, VirtualDom.Node msg )
renderEmpty context =
    ( context, empty )


textFromSingleClass : VirtualDom.Attribute msg
textFromSingleClass =
    classes [ Css.any, Css.text, Css.widthFill, Css.heightFill ]


textFromRowOrColClass : VirtualDom.Attribute msg
textFromRowOrColClass =
    classes [ Css.any, Css.text, Css.widthContent, Css.heightContent ]


renderText : String -> Layout -> Context -> ( Context, VirtualDom.Node msg )
renderText txt parent context =
    ( context
    , VirtualDom.node "div"
        [ if parent == Single then
            textFromSingleClass

          else
            textFromRowOrColClass
        ]
        [ VirtualDom.text txt ]
    )


layoutSingleClass : VirtualDom.Attribute msg
layoutSingleClass =
    classes [ Css.any, Css.single ]


layoutRowClass : VirtualDom.Attribute msg
layoutRowClass =
    classes [ Css.any, Css.row, Css.contentLeft, Css.contentCenterY ]


layoutColClass : VirtualDom.Attribute msg
layoutColClass =
    classes [ Css.any, Css.col, Css.contentLeft, Css.contentTop ]


layoutToClass : Layout -> VirtualDom.Attribute msg
layoutToClass layout =
    case layout of
        Single ->
            layoutSingleClass

        Row ->
            layoutRowClass

        Col ->
            layoutColClass


renderAlignContainer : String -> List String -> VirtualDom.Node msg -> VirtualDom.Node msg
renderAlignContainer tag cls vnode =
    VirtualDom.node tag
        [ classes (Css.container :: Css.any :: Css.single :: cls)
        ]
        [ vnode
        ]


wrapAlignContainer : Layout -> Config msg -> VirtualDom.Node msg -> VirtualDom.Node msg
wrapAlignContainer layout config vnode =
    case ( layout, config.alignX, config.alignY ) of
        ( Row, Just Middle, _ ) ->
            renderAlignContainer "s" [ Css.contentCenterY, Css.alignContainerCenterX ] vnode

        ( Row, Just End, _ ) ->
            renderAlignContainer "u" [ Css.contentCenterY, Css.alignContainerRight ] vnode

        ( Col, _, Just Middle ) ->
            renderAlignContainer "s" [ Css.alignContainerCenterY ] vnode

        ( Col, _, Just End ) ->
            renderAlignContainer "u" [ Css.alignContainerBottom ] vnode

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
        (layoutToClass layout :: attributes)
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


renderSelectors : Dict String String -> List ( String, VirtualDom.Node msg ) -> List ( String, VirtualDom.Node msg )
renderSelectors selectors nodes =
    Dict.foldr
        (\selector rules acc -> ( selector, VirtualDom.text (selector ++ "{" ++ rules ++ "}") ) :: acc)
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
        (classes [ Css.root, Css.any, Css.single ] :: attributes)
        [ VirtualDom.lazy Css.static ()
        , renderContext finalContext
        , vnode
        ]
