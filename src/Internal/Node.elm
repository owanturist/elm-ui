module Internal.Node exposing (Alignment(..), Layout(..), Length(..), Node(..), Prop(..), Style(..), render)

import Dict exposing (Dict)
import Internal.Box as Box exposing (Box)
import Json.Encode as Encode
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
    = Paddings (Box Int)


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
    }


initialContext : Context
initialContext =
    { paddings = Dict.empty
    }


type alias Config msg =
    { attributes : List (VirtualDom.Attribute msg)
    }


initialConfig : Config msg
initialConfig =
    { attributes = []
    }


applyStyle : Style -> ( Context, Config msg ) -> ( Context, Config msg )
applyStyle style ( context, config ) =
    case style of
        Paddings box ->
            let
                className =
                    Box.toClass "p" Box.int box

                css =
                    Box.toCss "padding" Box.px box
            in
            ( { context | paddings = Dict.insert className css context.paddings }
            , { config | attributes = class className :: config.attributes }
            )


applyPropToContext : Prop msg -> ( Context, Config msg ) -> ( Context, Config msg )
applyPropToContext prop cc =
    case prop of
        Attribute attr ->
            Debug.todo "Attribute"

        Selector selector ->
            Debug.todo "Selector"

        Styles style ->
            applyStyle style cc

        Batch many ->
            List.foldr applyPropToContext cc many


applyPropsToContext : List (Prop msg) -> Context -> ( Context, Config msg )
applyPropsToContext props context =
    List.foldr applyPropToContext ( context, initialConfig ) props


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
        ( nextContext, config ) =
            applyPropsToContext props context

        ( finalContext, child ) =
            renderHelp nextContext node
    in
    ( finalContext
    , VirtualDom.node tag
        (class "e" :: config.attributes)
        [ child ]
    )


renderBatchElement : String -> Context -> String -> List (Prop msg) -> List (Node msg) -> ( Context, VirtualDom.Node msg )
renderBatchElement className context tag props nodes =
    let
        ( nextContext, config ) =
            applyPropsToContext props context

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
        (class className :: config.attributes)
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


renderPaddings : Dict String String -> String
renderPaddings paddings =
    Dict.foldr (\id rules acc -> "." ++ id ++ curlyBraces rules ++ acc) "" paddings


renderContext : Context -> VirtualDom.Node msg
renderContext context =
    VirtualDom.node "style"
        []
        [ VirtualDom.text (renderPaddings context.paddings)
        ]


render : List (Prop msg) -> Node msg -> VirtualDom.Node msg
render props node =
    let
        ( rootContext, config ) =
            applyPropsToContext props initialContext

        ( finalContext, vnode ) =
            renderHelp rootContext node
    in
    VirtualDom.node "div"
        (class "ui" :: config.attributes)
        [ renderContext finalContext
        , vnode
        ]
