module Element exposing
    ( Attribute
    , Color
    , Common
    , Decoration
    , Element
    , Html
    , background
    , batch
    , col
    , color
    , el
    , empty
    , layout
    , none
    , padding
    , rgb
    , rgba
    , row
    , text
    )

import Internal.Color as Color
import Internal.Node as Internal
import VirtualDom


type alias Html msg =
    VirtualDom.Node msg


type alias Element msg =
    Internal.Node msg


empty : Element msg
empty =
    Internal.Empty


text : String -> Element msg
text =
    Internal.Text


el : List (Common {} msg) -> Element msg -> Element msg
el attributes child =
    Internal.Element "div"
        (List.map unwrapAttribute attributes)
        (Internal.Single child)


col : List (Common { spacing : () } msg) -> List (Element msg) -> Element msg
col attributes children =
    Internal.Element "div"
        (List.map unwrapAttribute attributes)
        (Internal.Col children)


row : List (Common { spacing : () } msg) -> List (Element msg) -> Element msg
row attributes children =
    Internal.Element "div"
        (List.map unwrapAttribute attributes)
        (Internal.Row children)



-- A T T R I B U T E


type Attribute support msg
    = Attribute (Internal.Prop msg)


type alias Common support msg =
    Decoration
        { support
            | width : ()
            , height : ()
            , padding : ()
            , align : ()
        }
        msg


type alias Decoration support msg =
    Attribute
        { support
            | transparent : ()
            , alpha : ()
            , move : ()
            , rotate : ()
            , scale : ()
            , background : ()
            , color : ()
        }
        msg


unwrapAttribute : Attribute support msg -> Internal.Prop msg
unwrapAttribute (Attribute prop) =
    prop


batch : List (Attribute support msg) -> Attribute support msg
batch attributes =
    case attributes of
        single :: [] ->
            single

        many ->
            Attribute (Internal.Batch (List.map unwrapAttribute many))


none : Attribute support msg
none =
    batch []



-- S I Z E


type alias Length =
    Internal.Length


shrink : Length
shrink =
    Internal.Shrink


fill : Length
fill =
    Internal.Portion 1


portion : Int -> Length
portion =
    Internal.Portion


px : Int -> Length
px =
    Internal.Px


minimum : Int -> Length -> Length
minimum =
    Internal.Minimum


maximum : Int -> Length -> Length
maximum =
    Internal.Maximum


width : Length -> Attribute { support | width : () } msg
width length =
    Debug.todo "width"


height : Length -> Attribute { support | height : () } msg
height length =
    Debug.todo "height"



-- P A D D I N G   A N D   S P A C I N G


padding : Int -> Int -> Int -> Int -> Attribute { support | padding : () } msg
padding t r b l =
    { top = t
    , right = r
    , bottom = b
    , left = l
    }
        |> Internal.Paddings
        |> Internal.Styles
        |> Attribute


spacing : Int -> Int -> Int -> Int -> Attribute { support | spacing : () } msg
spacing t r b l =
    Debug.todo "spacing"



-- A L I G M E N T


type Alignment support
    = Alignment Internal.Alignment


top : Alignment { support | top : () }
top =
    Alignment Internal.Start


bottom : Alignment { support | bottom : () }
bottom =
    Alignment Internal.End


left : Alignment { support | left : () }
left =
    Alignment Internal.Start


right : Alignment { support | right : () }
right =
    Alignment Internal.Start


center : Alignment { support | center : () }
center =
    Alignment Internal.Middle


align :
    Alignment { left : (), center : (), right : () }
    -> Alignment { top : (), center : (), bottom : () }
    -> Attribute { support | align : () } msg
align x y =
    Debug.todo "align"



-- T R A N S P A R E N C Y


pointer : Attribute { support | pointer : () } msg
pointer =
    Debug.todo "pointer"


transparent : Bool -> Attribute { support | transparent : () } msg
transparent x =
    Debug.todo "transparent"


alpha : Float -> Attribute { support | alpha : () } msg
alpha x =
    Debug.todo "alpha"



-- A D J U S T M E N T


move : Int -> Int -> Attribute { support | move : () } msg
move x y =
    Debug.todo "move"


rotate : Float -> Attribute { support | rotate : () } msg
rotate angle =
    Debug.todo "rotate"


scale : Float -> Attribute { support | scale : () } msg
scale angle =
    Debug.todo "scale"



-- C L I P P I N G   A N D   S C R O L L B A R S


clip : Attribute { support | clip : () } msg
clip =
    Debug.todo "clip"


scrollbars : Attribute { support | scrollbars : () } msg
scrollbars =
    Debug.todo "scrollbars"



-- C O L O R


type alias Color =
    Color.Color


rgb : Int -> Int -> Int -> Color
rgb r g b =
    Color.Rgba r g b 1


rgba : Int -> Int -> Int -> Int -> Color
rgba =
    Color.Rgba


background : Color -> Attribute { support | background : () } msg
background clr =
    clr
        |> Internal.Background
        |> Internal.Styles
        |> Attribute


color : Color -> Attribute { support | color : () } msg
color clr =
    clr
        |> Internal.Color
        |> Internal.Styles
        |> Attribute



-- T E M P O R A R Y   S T Y L I N G


transition :
    Int
    -> List (Decoration {} msg)
    -> Attribute { support | transition : () } msg
transition duration =
    transitionWith { duration = duration, delay = 0 }


transitionWith :
    { duration : Int
    , delay : Int
    }
    -> List (Decoration {} msg)
    -> Attribute { support | transition : () } msg
transitionWith { duration, delay } decorations =
    Debug.todo "transitionWith"


mouseOver : List (Decoration { transition : () } msg) -> Attribute support msg
mouseOver decorations =
    Debug.todo "mouseOver"


mouseDown : List (Decoration { transition : () } msg) -> Attribute support msg
mouseDown decorations =
    Debug.todo "mouseDown"


focused : List (Decoration { transition : () } msg) -> Attribute support msg
focused decorations =
    Debug.todo "focused"



-- R E N D E R I N G


layout : List (Common {} msg) -> Element msg -> Html msg
layout attributes element =
    Internal.render (List.map unwrapAttribute attributes) element
