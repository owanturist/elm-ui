module Internal.Node exposing
    ( Alignment(..)
    , Layout(..)
    , Length(..)
    , Node(..)
    , Prop(..)
    , Style(..)
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
    | Selector String
    | Styles Style
    | Batch (List (Prop msg))


type Style
    = Padding (Box Int)
    | Background Color
    | Color Color


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
    , colors : Dict String String
    }


initialContext : Context
initialContext =
    { paddings = Dict.empty
    , backgrounds = Dict.empty
    , colors = Dict.empty
    }


type alias Config msg =
    { attributes : List (VirtualDom.Attribute msg)
    , padding : Maybe (Box Int)
    , background : Maybe Color
    , color : Maybe Color
    }


initialConfig : Config msg
initialConfig =
    { attributes = []
    , padding = Nothing
    , background = Nothing
    , color = Nothing
    }


applyStyleToConfig : Style -> Config msg -> Config msg
applyStyleToConfig style config =
    case style of
        Padding box ->
            { config | padding = Maybe.Extra.or config.padding (Just box) }

        Background color ->
            { config | background = Maybe.Extra.or config.background (Just color) }

        Color color ->
            { config | color = Maybe.Extra.or config.color (Just color) }


applyPropToConfig : Prop msg -> Config msg -> Config msg
applyPropToConfig prop config =
    case prop of
        Attribute attr ->
            Debug.todo "Attribute"

        Selector selector ->
            Debug.todo "Selector"

        Styles style ->
            applyStyleToConfig style config

        Batch props ->
            List.foldr applyPropToConfig config props


type alias Acc msg =
    ( Context, List (VirtualDom.Attribute msg) )


applyConfigPadding : Box Int -> Acc msg -> Acc msg
applyConfigPadding padding ( context, attributes ) =
    let
        className =
            Box.toClass "p" Box.int padding
    in
    ( { context | paddings = Dict.insert className (Box.toCss "padding" Box.px padding) context.paddings }
    , class className :: attributes
    )


applyConfigBackground : Color -> Acc msg -> Acc msg
applyConfigBackground background ( context, attributes ) =
    let
        className =
            Color.toClass "bg" background
    in
    ( { context | backgrounds = Dict.insert className (Color.toCss "background-color" background) context.backgrounds }
    , class className :: attributes
    )


applyConfigColor : Color -> Acc msg -> Acc msg
applyConfigColor color ( context, attributes ) =
    let
        className =
            Color.toClass "c" color
    in
    ( { context | colors = Dict.insert className (Color.toCss "color" color) context.colors }
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
    [ Maybe.map applyConfigPadding config.padding
    , Maybe.map applyConfigBackground config.background
    , Maybe.map applyConfigColor config.color
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
        |> renderSelectors context.colors
        |> VirtualDom.keyedNode "style" []


staticCss : VirtualDom.Node msg
staticCss =
    VirtualDom.node "style" [] [ VirtualDom.text Static.css ]


render : List (Prop msg) -> Node msg -> VirtualDom.Node msg
render props node =
    let
        ( context, attributes ) =
            applyProps
                (Styles (Background (Color.Rgba 255 255 255 1))
                    :: Styles (Color (Color.Rgba 0 0 0 1))
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
