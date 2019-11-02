module Internal.Node exposing
    ( Alignment(..)
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
import Maybe.Extra
import VirtualDom


type Node msg
    = Empty
    | Text String
    | Element String (List (Prop msg)) (Layout msg)


type Layout msg
    = Single (Node msg)
    | Row (List (Node msg))
    | Col (List (Node msg))


type Prop msg
    = Attribute (VirtualDom.Attribute msg)
    | Batch (List (Prop msg))
    | Padding (Box Int)
    | Background Color
      -- F O N T S
    | FontColor Color
    | FontSize Int


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


type alias Context =
    { paddings : Dict String String
    , backgrounds : Dict String String

    -- F O N T S
    , fontColors : Dict String String
    , fontSizes : Dict String String
    }


initialContext : Context
initialContext =
    { paddings = Dict.empty
    , backgrounds = Dict.empty
    , fontColors = Dict.empty
    , fontSizes = Dict.empty
    }


type alias Config msg =
    { attributes : List (VirtualDom.Attribute msg)
    , padding : Maybe (Box Int)
    , background : Maybe Color

    -- F O N T S
    , fontColor : Maybe Color
    , fontSize : Maybe Int
    }


initialConfig : Config msg
initialConfig =
    { attributes = []
    , padding = Nothing
    , background = Nothing

    -- F O N T S
    , fontColor = Nothing
    , fontSize = Nothing
    }


applyPropToConfig : Prop msg -> Config msg -> Config msg
applyPropToConfig prop config =
    case prop of
        Attribute attr ->
            Debug.todo "Attribute"

        Padding box ->
            { config | padding = Maybe.Extra.or config.padding (Just box) }

        Background color ->
            { config | background = Maybe.Extra.or config.background (Just color) }

        -- F O N T S
        --
        FontColor color ->
            { config | fontColor = Maybe.Extra.or config.fontColor (Just color) }

        FontSize size ->
            { config | fontSize = Maybe.Extra.or config.fontSize (Just size) }

        Batch props ->
            List.foldr applyPropToConfig config props


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
    , VirtualDom.node tag
        (class "e" :: attributes)
        [ child ]
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
    , VirtualDom.node tag
        (class className :: attributes)
        children
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


curlyBraces : String -> String
curlyBraces str =
    "{" ++ str ++ "}"


renderSelectors : Dict String String -> List ( String, VirtualDom.Node msg ) -> List ( String, VirtualDom.Node msg )
renderSelectors paddings nodes =
    Dict.foldr (\id rules acc -> ( id, VirtualDom.text ("." ++ id ++ curlyBraces rules) ) :: acc) nodes paddings


renderContext : Context -> VirtualDom.Node msg
renderContext context =
    []
        |> renderSelectors context.paddings
        |> renderSelectors context.backgrounds
        |> renderSelectors context.fontColors
        |> renderSelectors context.fontSizes
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
