module Element exposing
    ( Attribute
    , Color
    , Common
    , Decoration
    , Element
    , Font
    , Html
    , background
    , batch
    , center
    , col
    , color
    , el
    , empty
    , fill
    , fontAlign
    , fontFamily
    , fontSize
    , height
    , justify
    , layout
    , left
    , letterSpacing
    , maximum
    , minimum
    , monospace
    , none
    , padding
    , portion
    , px
    , rgb
    , rgba
    , right
    , row
    , sansSerif
    , serif
    , shrink
    , text
    , typeface
    , width
    , wordSpacing
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
            , fontFamily : ()
            , fontAlign : ()
            , letterSpacing : ()
            , wordSpacing : ()
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
            , fontSize : ()
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
    Internal.Portion << max 1


px : Int -> Length
px =
    Internal.Px << max 0


minimum : Int -> Length -> Length
minimum x =
    Internal.Minimum (max 0 x)


maximum : Int -> Length -> Length
maximum x =
    Internal.Maximum (max 0 x)


width : Length -> Attribute { support | width : () } msg
width length =
    length
        |> Internal.Width
        |> Attribute


height : Length -> Attribute { support | height : () } msg
height length =
    length
        |> Internal.Height
        |> Attribute



-- P A D D I N G   A N D   S P A C I N G


padding : Int -> Int -> Int -> Int -> Attribute { support | padding : () } msg
padding t r b l =
    { top = max 0 t
    , right = max 0 r
    , bottom = max 0 b
    , left = max 0 l
    }
        |> Internal.Padding
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
    Alignment Internal.End


center : Alignment { support | center : () }
center =
    Alignment Internal.Middle


justify : Alignment { support | justify : () }
justify =
    Alignment Internal.Justify


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
        |> Attribute


color : Color -> Attribute { support | color : () } msg
color clr =
    clr
        |> Internal.FontColor
        |> Attribute



-- F O N T S


type alias Font =
    Internal.Font


fontSize : Int -> Attribute { support | fontSize : () } msg
fontSize size =
    size
        |> Internal.FontSize
        |> Attribute


typeface : String -> Font
typeface =
    Internal.TypeFace


serif : Font
serif =
    Internal.Serif


sansSerif : Font
sansSerif =
    Internal.SansSerif


monospace : Font
monospace =
    Internal.Monospace


fontFamily : List Font -> Attribute { support | fontFamily : () } msg
fontFamily fonts =
    if List.isEmpty fonts then
        none

    else
        fonts
            |> Internal.FontFamily
            |> Attribute


fontAlign : Alignment { left : (), right : (), center : (), justify : () } -> Attribute { support | fontAlign : () } msg
fontAlign (Alignment alignment) =
    alignment
        |> Internal.FontAlign
        |> Attribute


letterSpacing : Float -> Attribute { support | letterSpacing : () } msg
letterSpacing offset =
    offset
        |> Internal.LetterSpacing
        |> Attribute


wordSpacing : Float -> Attribute { support | wordSpacing : () } msg
wordSpacing offset =
    offset
        |> Internal.WordSpacing
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
