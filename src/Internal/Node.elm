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
    | Element String (List (Prop msg)) (Layout msg)


type Layout msg
    = Single (Node msg)
    | Row (List (Node msg))
    | Col (List (Node msg))


type Font
    = Serif
    | SansSerif
    | Monospace
    | TypeFace String


type Prop msg
    = Attribute (VirtualDom.Attribute msg)
    | Batch (List (Prop msg))
    | Padding (Box Int)
    | Background Color
      -- F O N T S
    | FontColor Color
    | FontSize Int
    | FontFamily (List Font)
    | FontAlign Alignment
    | FontDecoration String


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
    { paddings : Dict String String
    , backgrounds : Dict String String

    -- F O N T S
    , fontColors : Dict String String
    , fontSizes : Dict String String
    , fontFamilies : Dict String String
    }


initialContext : Context
initialContext =
    { paddings = Dict.empty
    , backgrounds = Dict.empty

    -- F O N T S
    , fontColors = Dict.empty
    , fontSizes = Dict.empty
    , fontFamilies = Dict.empty
    }


type alias Config msg =
    { attributes : List (VirtualDom.Attribute msg)
    , padding : Maybe (Box Int)
    , background : Maybe Color

    -- F O N T S
    , fontColor : Maybe Color
    , fontSize : Maybe Int
    , fontFamily : Maybe (List Font)
    , fontAlign : Maybe Alignment
    , fontDecoration : Maybe String
    }


initialConfig : Config msg
initialConfig =
    { attributes = []
    , padding = Nothing
    , background = Nothing

    -- F O N T S
    , fontColor = Nothing
    , fontSize = Nothing
    , fontFamily = Nothing
    , fontAlign = Nothing
    , fontDecoration = Nothing
    }


hash : String -> Int
hash =
    Murmur3.hashString 0


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

        Padding box ->
            if isNothing config.padding then
                { config | padding = Just box }

            else
                config

        Background color ->
            if isNothing config.background then
                { config | background = Just color }

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


type alias Acc msg =
    ( Context, List (VirtualDom.Attribute msg) )


applyPadding : Box Int -> Acc msg -> Acc msg
applyPadding padding ( context, attributes ) =
    let
        className =
            Box.toClass "p" String.fromInt padding
    in
    ( { context | paddings = Dict.insert className (Box.toCss "padding" px padding) context.paddings }
    , class className :: attributes
    )


applyBackground : Color -> Acc msg -> Acc msg
applyBackground background ( context, attributes ) =
    let
        className =
            Color.toClass "bg" background
    in
    ( { context | backgrounds = Dict.insert className (Color.toCss "background-color" background) context.backgrounds }
    , class className :: attributes
    )


applyFontColor : Color -> Acc msg -> Acc msg
applyFontColor color ( context, attributes ) =
    let
        className =
            Color.toClass "fc" color
    in
    ( { context | fontColors = Dict.insert className (Color.toCss "color" color) context.fontColors }
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
    ( { context | fontSizes = Dict.insert className css context.fontSizes }
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
            ("font-family:" ++ String.join "," names ++ ";")
                ++ "font-variant:normal;"
    in
    ( { context | fontFamilies = Dict.insert className css context.fontFamilies }
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


applyPropsFn : Maybe (Acc msg -> Acc msg) -> Acc msg -> Acc msg
applyPropsFn fn acc =
    fn
        |> Maybe.map ((|>) acc)
        |> Maybe.withDefault acc


applyProps : List (Prop msg) -> Context -> Acc msg
applyProps props context =
    let
        config =
            List.foldr applyPropToConfig initialConfig props
    in
    [ Maybe.map applyPadding config.padding
    , Maybe.map applyBackground config.background
    , Maybe.map applyFontColor config.fontColor
    , Maybe.map applyFontSize config.fontSize
    , Maybe.map applyFontFamily config.fontFamily
    , Maybe.map applyFontAlign config.fontAlign
    , Maybe.map applyFontDecoration config.fontDecoration
    ]
        |> List.foldr applyPropsFn ( context, config.attributes )


class : String -> VirtualDom.Attribute msg
class className =
    VirtualDom.property "className" (Encode.string className)


empty : VirtualDom.Node msg
empty =
    VirtualDom.text ""


renderEmpty : Context -> ( Context, VirtualDom.Node msg )
renderEmpty context =
    ( context, empty )


renderText : Context -> String -> ( Context, VirtualDom.Node msg )
renderText context txt =
    ( context, VirtualDom.text txt )


renderSingleElement : Context -> String -> List (Prop msg) -> Node msg -> ( Context, VirtualDom.Node msg )
renderSingleElement context tag props node =
    let
        ( nextContext, attributes ) =
            applyProps props context

        ( finalContext, child ) =
            renderHelp nextContext node
    in
    ( finalContext
    , VirtualDom.node tag (class "e" :: attributes) [ child ]
    )


renderBatchElement : String -> Context -> String -> List (Prop msg) -> List (Node msg) -> ( Context, VirtualDom.Node msg )
renderBatchElement className context tag props nodes =
    let
        ( nextContext, attributes ) =
            applyProps props context

        ( finalContext, children ) =
            List.foldr
                (\node ( contextAcc, childrenAcc ) ->
                    Tuple.mapSecond
                        (\child -> child :: childrenAcc)
                        (renderHelp contextAcc node)
                )
                ( nextContext, [] )
                nodes
    in
    ( finalContext
    , VirtualDom.node tag (class className :: attributes) children
    )


renderElement : Context -> String -> List (Prop msg) -> Layout msg -> ( Context, VirtualDom.Node msg )
renderElement context tag props layout =
    case layout of
        Single node ->
            renderSingleElement context tag props node

        Row nodes ->
            renderBatchElement "r" context tag props nodes

        Col nodes ->
            renderBatchElement "c" context tag props nodes


renderHelp : Context -> Node msg -> ( Context, VirtualDom.Node msg )
renderHelp context node =
    case node of
        Empty ->
            renderEmpty context

        Text txt ->
            renderText context txt

        Element tag props layout ->
            renderElement context tag props layout


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
    Dict.foldr (\id rules acc -> ( id, VirtualDom.text ("." ++ id ++ curlyBraces rules) ) :: acc) nodes selectors


renderContext : Context -> VirtualDom.Node msg
renderContext context =
    []
        |> renderSelectors context.paddings
        |> renderSelectors context.backgrounds
        |> renderSelectors context.fontColors
        |> renderSelectors context.fontSizes
        |> renderSelectors context.fontFamilies
        |> VirtualDom.keyedNode "style" []


staticCss : VirtualDom.Node msg
staticCss =
    VirtualDom.node "style" [] [ VirtualDom.text Static.css ]


render : List (Prop msg) -> Node msg -> VirtualDom.Node msg
render props node =
    let
        ( context, attributes ) =
            applyProps
                (Background (Color.Rgba 255 255 255 1)
                    :: FontColor (Color.Rgba 0 0 0 1)
                    :: FontSize 20
                    :: FontFamily
                        [ TypeFace "Open Sans"
                        , TypeFace "Helvetica"
                        , TypeFace "Verdana"
                        , SansSerif
                        ]
                    :: props
                )
                initialContext

        ( finalContext, vnode ) =
            renderHelp context node
    in
    VirtualDom.node "div"
        (class "ui s e" :: attributes)
        [ VirtualDom.lazy (always staticCss) ()
        , renderContext finalContext
        , vnode
        ]
