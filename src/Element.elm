module Element exposing
    ( Attribute
    , Color
    , Common
    , Decoration
    , Element
    , Font
    , Html
    , align
    , alignX
    , alignY
    , alpha
    , background
    , batch
    , bottom
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
    , paddingEach
    , paddingXY
    , paddings
    , pointer
    , portion
    , px
    , rgb
    , rgba
    , right
    , row
    , sansSerif
    , serif
    , shrink
    , spacing
    , text
    , top
    , typeface
    , width
    , wordSpacing
    )

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
    Internal.Element Internal.Single
        (List.map unwrapAttribute attributes)
        [ child ]


col : List (Common { spacing : () } msg) -> List (Element msg) -> Element msg
col attributes children =
    Internal.Element Internal.Col
        (List.map unwrapAttribute attributes)
        children


row : List (Common { spacing : () } msg) -> List (Element msg) -> Element msg
row attributes children =
    Internal.Element Internal.Row
        (List.map unwrapAttribute attributes)
        children



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
            , pointer : ()
        }
        msg


type alias Decoration support msg =
    Attribute
        { support
            | alpha : ()
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


paddings : Int -> Int -> Int -> Int -> Attribute { support | padding : () } msg
paddings t r b l =
    { t = max 0 t
    , r = max 0 r
    , b = max 0 b
    , l = max 0 l
    }
        |> Internal.Padding
        |> Attribute


padding : Int -> Attribute { support | padding : () } msg
padding pad =
    paddingXY pad pad


paddingXY : Int -> Int -> Attribute { support | padding : () } msg
paddingXY padX padY =
    paddings padX padY padX padY


paddingEach :
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }
    -> Attribute { support | padding : () } msg
paddingEach pads =
    paddings pads.top pads.right pads.bottom pads.left


spacing : Int -> Attribute { support | spacing : () } msg
spacing space =
    space
        |> Internal.Spacing
        |> Attribute



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
align (Alignment x) (Alignment y) =
    [ Internal.AlignX x
    , Internal.AlignY y
    ]
        |> Internal.Batch
        |> Attribute


alignX : Alignment { left : (), center : (), right : () } -> Attribute { support | align : () } msg
alignX (Alignment x) =
    Internal.AlignX x
        |> Attribute


alignY : Alignment { top : (), center : (), bottom : () } -> Attribute { support | align : () } msg
alignY (Alignment y) =
    Internal.AlignY y
        |> Attribute



-- T R A N S P A R E N C Y


pointer : Attribute { support | pointer : () } msg
pointer =
    Internal.Pointer
        |> Attribute


alpha : Float -> Attribute { support | alpha : () } msg
alpha x =
    clamp 0 1 x
        |> Internal.Opacity
        |> Attribute



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


type Color
    = Color Internal.Color


rgb : Int -> Int -> Int -> Color
rgb r g b =
    rgba r g b 1


rgba : Int -> Int -> Int -> Float -> Color
rgba r g b a =
    { r = clamp 0 255 r
    , g = clamp 0 255 g
    , b = clamp 0 255 b
    , a = clamp 0 1 a
    }
        |> Color


background : Color -> Attribute { support | background : () } msg
background (Color clr) =
    clr
        |> Internal.Background
        |> Attribute


color : Color -> Attribute { support | color : () } msg
color (Color clr) =
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
