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
    | Spacing Length
    | Wrapped Length
    | Padding Box
    | Width Length
    | Height Length
    | AlignX Alignment
    | AlignY Alignment
      -- D R E S S
    | Background Color
    | Opacity Float
    | Move Int Int
    | Scale Float
    | Rotate Float
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
    Dict String String


initialContext : Context
initialContext =
    Dict.empty


type alias Config msg =
    { attributes : List (VirtualDom.Attribute msg)

    -- G E O M E T R Y
    , spacing : Maybe Length
    , wrapped : Maybe Length
    , padding : Maybe Box
    , width : Maybe Length
    , height : Maybe Length
    , alignX : Maybe Alignment
    , alignY : Maybe Alignment

    -- D R E S S
    , background : Maybe Color
    , opacity : Maybe Float
    , move : Maybe ( Int, Int )
    , scale : Maybe Float
    , rotate : Maybe Float

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


initialConfig : Config msg
initialConfig =
    { attributes = []

    -- G E O M E T R Y
    , spacing = Nothing
    , padding = Nothing
    , width = Nothing
    , height = Nothing
    , alignX = Nothing
    , alignY = Nothing
    , wrapped = Nothing

    -- D R E S S
    , background = Nothing
    , opacity = Nothing
    , move = Nothing
    , scale = Nothing
    , rotate = Nothing

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


div : List (VirtualDom.Attribute msg) -> List (VirtualDom.Node msg) -> VirtualDom.Node msg
div =
    VirtualDom.node "div"


class : String -> VirtualDom.Attribute msg
class className =
    VirtualDom.property "className" (Encode.string className)


classes : List String -> VirtualDom.Attribute msg
classes classNames =
    class (String.join " " classNames)


isNothing : Maybe x -> Bool
isNothing =
    (==) Nothing


applyPropToConfig : Prop msg -> Config msg -> Config msg
applyPropToConfig prop config =
    case prop of
        Attribute attr ->
            Debug.todo "Attribute"

        Batch props ->
            List.foldr applyPropToConfig config props

        -- G E O M E T R Y
        Spacing space ->
            if isNothing config.spacing then
                { config | spacing = Just space }

            else
                config

        Wrapped space ->
            if isNothing config.wrapped then
                { config | wrapped = Just space }

            else
                config

        Padding box ->
            if isNothing config.padding then
                { config | padding = Just box }

            else
                config

        Width width ->
            if isNothing config.width then
                { config | width = Just width }

            else
                config

        Height height ->
            if isNothing config.height then
                { config | height = Just height }

            else
                config

        AlignX alignment ->
            if isNothing config.alignX then
                { config | alignX = Just alignment }

            else
                config

        AlignY alignment ->
            if isNothing config.alignY then
                { config | alignY = Just alignment }

            else
                config

        -- D R E S S
        --
        Background color ->
            if isNothing config.background then
                { config | background = Just color }

            else
                config

        Opacity o ->
            if isNothing config.opacity then
                { config | opacity = Just o }

            else
                config

        Move x y ->
            if isNothing config.move then
                { config | move = Just ( x, y ) }

            else
                config

        Scale n ->
            if isNothing config.scale then
                { config | scale = Just n }

            else
                config

        Rotate deg ->
            if isNothing config.rotate then
                { config | rotate = Just deg }

            else
                config

        -- F O N T S
        --
        FontColor color ->
            if isNothing config.fontColor then
                { config | fontColor = Just color }

            else
                config

        FontSize size ->
            if isNothing config.fontSize then
                { config | fontSize = Just size }

            else
                config

        FontFamily family ->
            if isNothing config.fontFamily then
                { config | fontFamily = Just family }

            else
                config

        FontAlign align ->
            if isNothing config.fontAlign then
                { config | fontAlign = Just align }

            else
                config

        FontDecoration decoration ->
            if isNothing config.fontDecoration then
                { config | fontDecoration = Just decoration }

            else
                config

        LetterSpacing spacing ->
            if isNothing config.letterSpacing then
                { config | letterSpacing = Just spacing }

            else
                config

        WordSpacing spacing ->
            if isNothing config.wordSpacing then
                { config | wordSpacing = Just spacing }

            else
                config

        -- S P E C I A L   S T A T E
        Pointer ->
            { config | pointer = True }


applyPropsToConfig : List (Prop msg) -> Config msg
applyPropsToConfig =
    List.foldr applyPropToConfig initialConfig


type alias Acc msg =
    ( Context, List (VirtualDom.Attribute msg) )


applyEvenlySpacing : Acc msg -> Acc msg
applyEvenlySpacing ( context, attributes ) =
    ( context
    , class Css.spaceEvenly :: attributes
    )


applyColPxSpacing : Int -> Acc msg -> Acc msg
applyColPxSpacing spaceY ( context, attributes ) =
    let
        ( className, css ) =
            Css.spacingCol spaceY

        selector =
            Css.dot Css.col ++ Css.dot className ++ ">" ++ Css.dot Css.any ++ "+" ++ Css.dot Css.any
    in
    ( Dict.insert selector css context
    , class className :: attributes
    )


applyRowPxSpacing : Int -> Acc msg -> Acc msg
applyRowPxSpacing spaceX ( context, attributes ) =
    let
        ( className, css ) =
            Css.spacingRow spaceX

        selector =
            Css.dot Css.row ++ Css.dot className ++ ">" ++ Css.dot Css.any ++ "+" ++ Css.dot Css.any
    in
    ( Dict.insert selector css context
    , class className :: attributes
    )


applyWrappedRowSpacing : Int -> Maybe Int -> Acc msg -> Acc msg
applyWrappedRowSpacing spaceX maybeSpaceY ( context, attributes ) =
    let
        ( className, parentCss, childCss ) =
            case maybeSpaceY of
                Nothing ->
                    Css.spacingWrappedEvenlyRow spaceX

                Just spaceY ->
                    Css.spacingWrappedPxRow spaceX spaceY

        parentSelector =
            Css.dot Css.single ++ Css.dot className ++ ">" ++ Css.dot Css.wrapped

        childSelector =
            parentSelector ++ ">" ++ Css.dot Css.any
    in
    ( context
        |> Dict.insert parentSelector parentCss
        |> Dict.insert childSelector childCss
    , class className :: attributes
    )


{-| Makes spacing for Col element. Px 0 is ignored.

Possible cases:

  - Col + spacing px
  - Col + spacing evenly

-}
applyColSpacing : Length -> Acc msg -> Acc msg
applyColSpacing lengthY acc =
    case lengthY of
        Px 0 ->
            acc

        Px spaceY ->
            applyColPxSpacing spaceY acc

        -- the rest treats as evenly
        _ ->
            applyEvenlySpacing acc


{-| Makes spacing for Row element. Px 0 is ignored.

Possible cases:

  - Row + spacing px
  - Row + spacing evenly

-}
applyRowSpacing : Length -> Acc msg -> Acc msg
applyRowSpacing lengthX acc =
    case lengthX of
        Px 0 ->
            acc

        Px spaceX ->
            applyRowPxSpacing spaceX acc

        -- the rest treats as evenly
        _ ->
            applyEvenlySpacing acc


{-| Makes spacing for wrapped Row element when spacing is unset. Px 0 is ignored.

Possible cases:

  - wrapped Row + spacing px
  - wrapped Row + spacing evenly

-}
applyWrappedSpacing : Length -> Acc msg -> Acc msg
applyWrappedSpacing lengthY acc =
    case lengthY of
        Px spaceY ->
            applyWrappedRowSpacing 0 (Just spaceY) acc

        -- do nothing for evenly in this case
        _ ->
            acc


{-| Makes spacing for Row element. Px 0 is ignored.

Possible cases:

  - Row + spacing px - wrapped Row spacing px
  - Row + spacing px - wrapped Row evenly
  - Row + evenly - wrapped Row spacing px
  - Row + evenly - wrapped Row evenly

-}
applyBothRowsSpacing : Length -> Length -> Acc msg -> Acc msg
applyBothRowsSpacing lengthX lengthY acc =
    case ( lengthX, lengthY ) of
        ( Px spaceX, Px spaceY ) ->
            applyWrappedRowSpacing spaceX (Just spaceY) acc

        ( Px spaceX, _ ) ->
            applyWrappedRowSpacing spaceX Nothing acc

        ( _, Px spaceY ) ->
            acc
                |> applyWrappedRowSpacing 0 (Just spaceY)
                |> applyEvenlySpacing

        -- the rest treats as evenly
        _ ->
            applyEvenlySpacing acc


applySpacing : Layout -> Maybe Length -> Maybe Length -> Acc msg -> Acc msg
applySpacing layout spacing wrapped acc =
    case ( layout, spacing, wrapped ) of
        -- bypass for Single
        ( Single, _, _ ) ->
            acc

        ( Col, Just lengthY, _ ) ->
            applyColSpacing lengthY acc

        ( Row, Just lengthX, Nothing ) ->
            applyRowSpacing lengthX acc

        ( Row, Nothing, Just lengthY ) ->
            applyWrappedSpacing lengthY acc

        ( Row, Just lengthX, Just lengthY ) ->
            applyBothRowsSpacing lengthX lengthY acc

        _ ->
            acc


applyPadding : Box -> Acc msg -> Acc msg
applyPadding { t, r, b, l } ( context, attributes ) =
    let
        ( className, css ) =
            Css.padding t r b l
    in
    ( Dict.insert (Css.dot className) css context
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
            ( Dict.insert (Css.dot Css.row ++ ">" ++ Css.dot className) css context
            , classes [ Css.widthFillPortion, className ] :: attributes
            )

        Px x ->
            let
                ( className, css ) =
                    Css.widthPx x
            in
            ( Dict.insert (Css.dot className) css context
            , classes [ Css.widthExact, className ] :: attributes
            )

        Minimum x subLength ->
            let
                ( className, css ) =
                    Css.widthMin x
            in
            applyWidth subLength
                ( Dict.insert (Css.dot className) css context
                , class className :: attributes
                )

        Maximum x subLength ->
            let
                ( className, css ) =
                    Css.widthMax x
            in
            applyWidth subLength
                ( Dict.insert (Css.dot className) css context
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
            ( Dict.insert (Css.dot Css.col ++ ">" ++ Css.dot className) css context
            , classes [ Css.heightFillPortion, className ] :: attributes
            )

        Px x ->
            let
                ( className, css ) =
                    Css.heightPx x
            in
            ( Dict.insert (Css.dot className) css context
            , classes [ Css.heightExact, className ] :: attributes
            )

        Minimum x subLength ->
            let
                ( className, css ) =
                    Css.heightMin x
            in
            applyHeight subLength
                ( Dict.insert (Css.dot className) css context
                , class className :: attributes
                )

        Maximum x subLength ->
            let
                ( className, css ) =
                    Css.heightMax x
            in
            applyHeight subLength
                ( Dict.insert (Css.dot className) css context
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
    ( Dict.insert (Css.dot className) css context
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
        ( Dict.insert (Css.dot className) css context
        , class className :: attributes
        )


applyTransform : Maybe ( Int, Int ) -> Maybe Float -> Maybe Float -> Acc msg -> Acc msg
applyTransform coords n deg ( context, attributes ) =
    case Css.transform coords n deg of
        Nothing ->
            ( context, attributes )

        Just ( className, css ) ->
            ( Dict.insert (Css.dot className) css context
            , class className :: attributes
            )


applyFontColor : Color -> Acc msg -> Acc msg
applyFontColor { r, g, b, a } ( context, attributes ) =
    let
        ( className, css ) =
            Css.fontColor r g b a
    in
    ( Dict.insert (Css.dot className) css context
    , class className :: attributes
    )


applyFontSize : Int -> Acc msg -> Acc msg
applyFontSize size ( context, attributes ) =
    let
        ( className, css ) =
            Css.fontSize size
    in
    ( Dict.insert (Css.dot className) css context
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
    ( Dict.insert (Css.dot className) css context
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
    ( Dict.insert (Css.dot className) css context
    , class className :: attributes
    )


applyWordSpacing : Float -> Acc msg -> Acc msg
applyWordSpacing spacing ( context, attributes ) =
    let
        ( className, css ) =
            Css.wordSpacing spacing
    in
    ( Dict.insert (Css.dot className) css context
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


applyOptional : (x -> Acc msg -> Acc msg) -> Maybe x -> Acc msg -> Acc msg
applyOptional fn x =
    Maybe.withDefault identity (Maybe.map fn x)


applyConfigToContext : Layout -> Config msg -> Context -> Acc msg
applyConfigToContext layout config context =
    ( context, config.attributes )
        |> applySpacing layout config.spacing config.wrapped
        |> applyOptional applyPadding config.padding
        |> applyOptional applyWidth config.width
        |> applyOptional applyHeight config.height
        |> applyOptional applyAlignX config.alignX
        |> applyOptional applyAlignY config.alignY
        |> applyOptional applyBackground config.background
        |> applyOptional applyOpacity config.opacity
        |> applyTransform config.move config.scale config.rotate
        |> applyOptional applyFontColor config.fontColor
        |> applyOptional applyFontSize config.fontSize
        |> applyOptional applyFontFamily config.fontFamily
        |> applyOptional applyFontAlign config.fontAlign
        |> applyOptional applyFontDecoration config.fontDecoration
        |> applyOptional applyLetterSpacing config.letterSpacing
        |> applyOptional applyWordSpacing config.wordSpacing
        |> applyPointer config.pointer


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
    , div
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
            applyPropsToConfig (Width Shrink :: Height Shrink :: props)

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

        vnode =
            if isNothing config.wrapped then
                div (layoutToClass layout :: attributes) children

            else
                div (layoutSingleClass :: attributes)
                    [ div [ layoutRowClass, class Css.wrapped ] children
                    ]
    in
    ( finalContext
    , vnode
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


renderContext : Context -> VirtualDom.Node msg
renderContext context =
    Dict.foldr
        (\selector rules acc -> ( selector, VirtualDom.text (selector ++ "{" ++ rules ++ "}") ) :: acc)
        []
        context
        |> VirtualDom.keyedNode "style" []


render : List (Prop msg) -> Node msg -> VirtualDom.Node msg
render props node =
    let
        config =
            Background (Color 255 255 255 1)
                :: FontColor (Color 0 0 0 1)
                :: FontSize 20
                :: FontFamily [ TypeFace "Open Sans", TypeFace "Helvetica", TypeFace "Verdana", SansSerif ]
                :: props
                |> applyPropsToConfig

        ( context, attributes ) =
            applyConfigToContext Single config initialContext

        ( finalContext, vnode ) =
            renderHelp Single context node
    in
    div
        (classes [ Css.root, Css.any, Css.single ] :: attributes)
        [ VirtualDom.lazy Css.static ()
        , renderContext finalContext
        , vnode
        ]
