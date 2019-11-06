module Element exposing
    ( Attribute
    , Color
    , Common
    , Decoration
    , Element
    , Font
    , Html
    , Property
    , align
    , alignX
    , alignY
    , alpha
    , background
    , batch
    , bottom
    , center
    , clip
    , clipX
    , clipY
    , col
    , color
    , download
    , downloadAs
    , el
    , empty
    , evenly
    , explain
    , fill
    , fontAlign
    , fontFamily
    , fontSize
    , height
    , image
    , justify
    , layout
    , left
    , letterSpacing
    , link
    , linkNewTab
    , maximum
    , minimum
    , monospace
    , move
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
    , rotate
    , row
    , sansSerif
    , scale
    , scroll
    , scrollX
    , scrollY
    , serif
    , shrink
    , spacing
    , text
    , top
    , typeface
    , width
    , wordSpacing
    , wrapped
    )

import Internal.Node as Internal
import VirtualDom


roundFloat : Int -> Float -> Float
roundFloat precision num =
    let
        n =
            toFloat (10 ^ precision)
    in
    toFloat (round (num * n)) / n



--


type alias Html msg =
    VirtualDom.Node msg


type alias Attribute msg =
    VirtualDom.Attribute msg


type alias Element msg =
    Internal.Node msg


empty : Element msg
empty =
    Internal.Empty


text : String -> Element msg
text =
    Internal.Text


el : List (Common { link : () } msg) -> Element msg -> Element msg
el attributes child =
    Internal.Element Internal.Single
        (List.map unwrapAttribute attributes)
        [ child ]


col : List (Common { link : (), spacing : () } msg) -> List (Element msg) -> Element msg
col attributes children =
    Internal.Element Internal.Col
        (List.map unwrapAttribute attributes)
        children


row : List (Common { link : (), spacing : (), wrapped : () } msg) -> List (Element msg) -> Element msg
row attributes children =
    Internal.Element Internal.Row
        (List.map unwrapAttribute attributes)
        children


paragraph : List (Common { link : (), spacing : () } msg) -> List (Element msg) -> Element msg
paragraph =
    Debug.todo "paragraph"


textColumn : List (Common { link : (), spacing : () } msg) -> List (Element msg) -> Element msg
textColumn =
    Debug.todo "textColumn"


type alias Column view msg =
    { header : Element msg
    , width : Length { shrink : (), fill : (), portion : (), px : () }
    , view : view
    }


table : List (Common { spacing : () } msg) -> List (Column (record -> Element msg) msg) -> List record -> Element msg
table =
    Debug.todo "table"


indexedTable : List (Common { spacing : () } msg) -> List (Column (Int -> record -> Element msg) msg) -> List record -> Element msg
indexedTable =
    Debug.todo "indexedTable"


image :
    List (Common { link : () } msg)
    ->
        { description : String
        , src : String
        }
    -> Element msg
image attributes { description, src } =
    Internal.Image description
        src
        (List.map unwrapAttribute attributes)


link : String -> Property { support | link : () } msg
link url =
    url
        |> Internal.SameTabLink
        |> Internal.Url
        |> Property


linkNewTab : String -> Property { support | link : () } msg
linkNewTab url =
    url
        |> Internal.NewTabLink
        |> Internal.Url
        |> Property


download : String -> Property { support | link : () } msg
download url =
    url
        |> Internal.DownloadFile ""
        |> Internal.Url
        |> Property


downloadAs :
    { url : String
    , filename : String
    }
    -> Property { support | link : () } msg
downloadAs { url, filename } =
    Internal.DownloadFile filename url
        |> Internal.Url
        |> Property



-- A T T R I B U T E


type Property support msg
    = Property (Internal.Prop msg)


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
            , scroll : ()
            , clip : ()
        }
        msg


type alias Decoration support msg =
    Property
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


unwrapAttribute : Property support msg -> Internal.Prop msg
unwrapAttribute (Property prop) =
    prop


batch : List (Property support msg) -> Property support msg
batch attributes =
    case attributes of
        single :: [] ->
            single

        many ->
            Property (Internal.Batch (List.map unwrapAttribute many))


none : Property support msg
none =
    batch []



-- S I Z E


type Length support
    = Length Internal.Length


evenly : Length { support | evenly : () }
evenly =
    Internal.Shrink
        |> Length


shrink : Length { support | shrink : () }
shrink =
    Internal.Shrink
        |> Length


fill : Length { support | fill : () }
fill =
    Internal.Portion 1
        |> Length


portion : Int -> Length { support | portion : () }
portion n =
    max 1 n
        |> Internal.Portion
        |> Length


px : Int -> Length { support | px : () }
px x =
    max 0 x
        |> Internal.Px
        |> Length


minimum : Int -> Length { shrink : (), fill : (), portion : () } -> Length { support | shrink : (), fill : (), portion : () }
minimum x (Length length) =
    Internal.Minimum (max 0 x) length
        |> Length


maximum : Int -> Length { shrink : (), fill : (), portion : () } -> Length { support | shrink : (), fill : (), portion : () }
maximum x (Length length) =
    Internal.Maximum (max 0 x) length
        |> Length


width : Length { shrink : (), fill : (), portion : (), px : () } -> Property { support | width : () } msg
width (Length length) =
    length
        |> Internal.Width
        |> Property


height : Length { shrink : (), fill : (), portion : (), px : () } -> Property { support | height : () } msg
height (Length length) =
    length
        |> Internal.Height
        |> Property



-- D E B U G G I N G


type alias Todo =
    String -> Never


explain : Todo -> Property support msg
explain _ =
    Internal.Explain
        |> Property



-- P A D D I N G   A N D   S P A C I N G


paddings : Int -> Int -> Int -> Int -> Property { support | padding : () } msg
paddings t r b l =
    { t = max 0 t
    , r = max 0 r
    , b = max 0 b
    , l = max 0 l
    }
        |> Internal.Padding
        |> Property


padding : Int -> Property { support | padding : () } msg
padding pad =
    paddingXY pad pad


paddingXY : Int -> Int -> Property { support | padding : () } msg
paddingXY padX padY =
    paddings padY padX padY padX


paddingEach :
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }
    -> Property { support | padding : () } msg
paddingEach pads =
    paddings pads.top pads.right pads.bottom pads.left


spacing : Length { evenly : (), px : () } -> Property { support | spacing : () } msg
spacing (Length space) =
    space
        |> Internal.Spacing
        |> Property


wrapped : Length { evenly : (), px : () } -> Property { support | wrapped : () } msg
wrapped (Length spaceY) =
    spaceY
        |> Internal.Wrapped
        |> Property



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
    -> Property { support | align : () } msg
align (Alignment x) (Alignment y) =
    [ Internal.AlignX x
    , Internal.AlignY y
    ]
        |> Internal.Batch
        |> Property


alignX : Alignment { left : (), center : (), right : () } -> Property { support | align : () } msg
alignX (Alignment x) =
    Internal.AlignX x
        |> Property


alignY : Alignment { top : (), center : (), bottom : () } -> Property { support | align : () } msg
alignY (Alignment y) =
    Internal.AlignY y
        |> Property



-- T R A N S P A R E N C Y


pointer : Property { support | pointer : () } msg
pointer =
    Internal.Pointer
        |> Property


alpha : Float -> Property { support | alpha : () } msg
alpha x =
    x
        |> roundFloat 3
        |> clamp 0 1
        |> Internal.Opacity
        |> Property



-- A D J U S T M E N T


move : Int -> Int -> Property { support | move : () } msg
move x y =
    Internal.Move x y
        |> Property


rotate : Float -> Property { support | rotate : () } msg
rotate deg =
    let
        n =
            floor (deg / 360)
    in
    (deg - toFloat (n * 360))
        |> roundFloat 1
        |> Internal.Rotate
        |> Property


scale : Float -> Property { support | scale : () } msg
scale n =
    n
        |> roundFloat 2
        |> Internal.Scale
        |> Property



-- C L I P P I N G   A N D   S C R O L L B A R S


clip : Property { support | clip : () } msg
clip =
    batch [ clipX, clipY ]


clipX : Property { support | clip : () } msg
clipX =
    Internal.Clip
        |> Internal.OverflowX
        |> Property


clipY : Property { support | clip : () } msg
clipY =
    Internal.Clip
        |> Internal.OverflowY
        |> Property


scroll : Property { support | scroll : () } msg
scroll =
    batch [ scrollX, scrollY ]


scrollX : Property { support | scroll : () } msg
scrollX =
    Internal.Scroll
        |> Internal.OverflowX
        |> Property


scrollY : Property { support | scroll : () } msg
scrollY =
    Internal.Scroll
        |> Internal.OverflowY
        |> Property



-- N E A R B Y   E L E M E N T S


above : Element msg -> Property { support | above : () } msg
above =
    Debug.todo "above"


onRight : Element msg -> Property { support | onRight : () } msg
onRight =
    Debug.todo "onRight"


below : Element msg -> Property { support | below : () } msg
below =
    Debug.todo "below"


onLeft : Element msg -> Property { support | onLeft : () } msg
onLeft =
    Debug.todo "onLeft"


inFront : Element msg -> Property { support | inFront : () } msg
inFront =
    Debug.todo "inFront"


behind : Element msg -> Property { support | behind : () } msg
behind =
    Debug.todo "behind"



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
    , a = clamp 0 1 (roundFloat 3 a)
    }
        |> Color



-- B A C K G R O U N D


background : Color -> Property { support | background : () } msg
background (Color clr) =
    clr
        |> Internal.Background
        |> Property


gradient : Float -> List Color -> Property { support | gradient : () } msg
gradient =
    Debug.todo "gradient"


cover : String -> Property { support | cover : () } msg
cover =
    Debug.todo "cover"


contain : String -> Property { support | contain : () } msg
contain =
    Debug.todo "contain"


tiled : String -> Property { support | tiled : () } msg
tiled =
    Debug.todo "tiled"


tiledX : String -> Property { support | tiled : () } msg
tiledX =
    Debug.todo "tiledX"


tiledY : String -> Property { support | tiled : () } msg
tiledY =
    Debug.todo "tiledY"



-- B O R D E R


type Border
    = Border


solid : Border
solid =
    Border


dashed : Border
dashed =
    Border


dotted : Border
dotted =
    Border


rounded : Property { support | rounded : () } msg
rounded =
    Debug.todo "rounded"


border : Int -> Border -> Color -> Property { support | border : () } msg
border =
    Debug.todo "border"


borderTop : Int -> Border -> Color -> Property { support | border : () } msg
borderTop =
    Debug.todo "borderTop"


borderRight : Int -> Border -> Color -> Property { support | border : () } msg
borderRight =
    Debug.todo "borderRight"


borderBottom : Int -> Border -> Color -> Property { support | border : () } msg
borderBottom =
    Debug.todo "borderBottom"


borderLeft : Int -> Border -> Color -> Property { support | border : () } msg
borderLeft =
    Debug.todo "borderLeft"


borderWidth : Int -> Property { support | borderWidth : () } msg
borderWidth x =
    borderWidthXY x x


borderWidthXY : Int -> Int -> Property { support | borderWidth : () } msg
borderWidthXY x y =
    bordersWidth y x y x


bordersWidth : Int -> Int -> Int -> Int -> Property { support | borderWidth : () } msg
bordersWidth =
    Debug.todo "bordersWidth"


borderWidthEach :
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }
    -> Property { support | borderWidth : () } msg
borderWidthEach borders =
    bordersWidth borders.top borders.right borders.bottom borders.left


borderStyle : Border -> Property { support | borderStyle : () } msg
borderStyle x =
    borderStyleXY x x


borderStyleXY : Border -> Border -> Property { support | borderStyle : () } msg
borderStyleXY x y =
    bordersStyle y x y x


bordersStyle : Border -> Border -> Border -> Border -> Property { support | borderStyle : () } msg
bordersStyle =
    Debug.todo "bordersWidth"


borderStyleEach :
    { top : Border
    , right : Border
    , bottom : Border
    , left : Border
    }
    -> Property { support | borderStyle : () } msg
borderStyleEach borders =
    bordersStyle borders.top borders.right borders.bottom borders.left


borderColor : Color -> Property { support | borderColor : () } msg
borderColor x =
    borderColorXY x x


borderColorXY : Color -> Color -> Property { support | borderColor : () } msg
borderColorXY x y =
    bordersColor y x y x


bordersColor : Color -> Color -> Color -> Color -> Property { support | borderColor : () } msg
bordersColor =
    Debug.todo "bordersWidth"


borderColorEach :
    { top : Color
    , right : Color
    , bottom : Color
    , left : Color
    }
    -> Property { support | borderColor : () } msg
borderColorEach borders =
    bordersColor borders.top borders.right borders.bottom borders.left



-- S H A D O W S


shadow : Int -> Int -> Int -> Int -> Color -> Property { support | shadow : () } msg
shadow x y blur size clr =
    Debug.todo "shadow"


glow : Int -> Color -> Property { support | glow : () } msg
glow blur clr =
    Debug.todo "shadow"


inner : Property { shadow : (), glow : () } msg -> Property { support | shadow : (), glow : () } msg
inner =
    Debug.todo "inner"



-- F O N T S


type alias Font =
    Internal.Font


color : Color -> Property { support | color : () } msg
color (Color clr) =
    clr
        |> Internal.FontColor
        |> Property


fontSize : Int -> Property { support | fontSize : () } msg
fontSize size =
    size
        |> Internal.FontSize
        |> Property


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


fontFamily : List Font -> Property { support | fontFamily : () } msg
fontFamily fonts =
    if List.isEmpty fonts then
        none

    else
        fonts
            |> Internal.FontFamily
            |> Property


fontAlign : Alignment { left : (), right : (), center : (), justify : () } -> Property { support | fontAlign : () } msg
fontAlign (Alignment alignment) =
    alignment
        |> Internal.FontAlign
        |> Property


letterSpacing : Float -> Property { support | letterSpacing : () } msg
letterSpacing offset =
    offset
        |> roundFloat 1
        |> Internal.LetterSpacing
        |> Property


wordSpacing : Float -> Property { support | wordSpacing : () } msg
wordSpacing offset =
    offset
        |> roundFloat 1
        |> Internal.WordSpacing
        |> Property


underline : Property { support | underline : () } msg
underline =
    Debug.todo "underline"


strike : Property { support | strike : () } msg
strike =
    Debug.todo "strike"


italic : Property { support | italic : () } msg
italic =
    Debug.todo "italic"


lightest : Property { support | fontWeight : () } msg
lightest =
    Debug.todo "fontWeight: 100"


extralight : Property { support | fontWeight : () } msg
extralight =
    Debug.todo "fontWeight: 200"


light : Property { support | fontWeight : () } msg
light =
    Debug.todo "fontWeight: 300"


normal : Property { support | fontWeight : () } msg
normal =
    Debug.todo "fontWeight: 400"


medium : Property { support | fontWeight : () } msg
medium =
    Debug.todo "fontWeight: 500"


semibold : Property { support | fontWeight : () } msg
semibold =
    Debug.todo "fontWeight: 600"


bold : Property { support | fontWeight : () } msg
bold =
    Debug.todo "fontWeight: 700"


extrabold : Property { support | fontWeight : () } msg
extrabold =
    Debug.todo "fontWeight: 800"


boldest : Property { support | fontWeight : () } msg
boldest =
    Debug.todo "fontWeight: 900"


{-| Resets underline, strike, italic and fontWeight
-}
unstyled : Property { support | unstyled : () } msg
unstyled =
    Debug.todo "unstyled"


type Variant
    = Variant


smallCaps : Variant
smallCaps =
    Variant


slashedZero : Variant
slashedZero =
    Variant


ligatures : Variant
ligatures =
    Variant


ordinal : Variant
ordinal =
    Variant


tabularNumbers : Variant
tabularNumbers =
    Variant


stackedFractions : Variant
stackedFractions =
    Variant


diagonalFractions : Variant
diagonalFractions =
    Variant


swash : Int -> Variant
swash _ =
    Variant


feature : String -> Bool -> Variant
feature _ _ =
    Variant


indexed : String -> Int -> Variant
indexed _ _ =
    Variant


fontVariants : List Variant -> Property { support | fontVariants : () } msg
fontVariants =
    Debug.todo "fontVariants"


fontShadow : Int -> Int -> Int -> Color -> Property { support | fontShadow : () } msg
fontShadow =
    Debug.todo "fontShadow"


fontGlow : Int -> Color -> Property { support | fontGlow : () } msg
fontGlow =
    Debug.todo "fontGlow"



-- T E M P O R A R Y   S T Y L I N G


transition : Int -> Decoration {} msg -> Property {} msg
transition duration =
    transitionWith { duration = duration, delay = 0 }


transitionWith :
    { duration : Int
    , delay : Int
    }
    -> Decoration {} msg
    -> Property {} msg
transitionWith { duration, delay } decorations =
    Debug.todo "transitionWith"


mouseOver : List (Decoration {} msg) -> Property support msg
mouseOver decorations =
    Debug.todo "mouseOver"


mouseDown : List (Decoration {} msg) -> Property support msg
mouseDown decorations =
    Debug.todo "mouseDown"


focused : List (Decoration {} msg) -> Property support msg
focused decorations =
    Debug.todo "focused"



-- E V E N T S


onClick : msg -> Property { support | onClick : () } msg
onClick msg =
    Debug.todo "onClick"


onDoubleClick : msg -> Property { support | onDoubleClick : () } msg
onDoubleClick msg =
    Debug.todo "onDoubleClick"


onMouseDown : msg -> Property { support | onMouseDown : () } msg
onMouseDown msg =
    Debug.todo "onMouseDown"


onMouseUp : msg -> Property { support | onMouseUp : () } msg
onMouseUp msg =
    Debug.todo "onMouseUp"


onMouseEnter : msg -> Property { support | onMouseEnter : () } msg
onMouseEnter msg =
    Debug.todo "onMouseEnter"


onMouseLeave : msg -> Property { support | onMouseLeave : () } msg
onMouseLeave msg =
    Debug.todo "onMouseLeave"


onMouseMove : msg -> Property { support | onMouseMove : () } msg
onMouseMove msg =
    Debug.todo "onMouseMove"


onFocus : msg -> Property { support | onFocus : () } msg
onFocus msg =
    Debug.todo "onFocus"


onBlur : msg -> Property { support | onBlur : () } msg
onBlur msg =
    Debug.todo "onBlur"


onEnter : msg -> Property { support | onEnter : () } msg
onEnter msg =
    Debug.todo "onEnter"


onSpace : msg -> Property { support | onSpace : () } msg
onSpace msg =
    Debug.todo "onSpace"


onInput : (String -> msg) -> Property { support | onInput : () } msg
onInput tagger =
    Debug.todo "onInput"


onCheck : (Bool -> msg) -> Property { support | onCheck : () } msg
onCheck tagger =
    Debug.todo "onCheck"



-- K E Y E D


key : String -> Property { support | key : () } msg
key =
    Debug.todo "key"



-- L A Z Y


lazy : (a -> Element msg) -> a -> Element msg
lazy =
    Debug.todo "lazy"


lazy2 : (a -> b -> Element msg) -> a -> b -> Element msg
lazy2 =
    Debug.todo "lazy2"


lazy3 : (a -> b -> c -> Element msg) -> a -> b -> c -> Element msg
lazy3 =
    Debug.todo "lazy3"


lazy4 : (a -> b -> c -> d -> Element msg) -> a -> b -> c -> d -> Element msg
lazy4 =
    Debug.todo "lazy4"


lazy5 : (a -> b -> c -> d -> e -> Element msg) -> a -> b -> c -> d -> e -> Element msg
lazy5 =
    Debug.todo "lazy5"



-- R E G I O N


main_ : Property { support | main : () } msg
main_ =
    Debug.todo "main"


nav : Property { support | nav : () } msg
nav =
    Debug.todo "nav"


h1 : Property { support | h1 : () } msg
h1 =
    Debug.todo "h1"


h2 : Property { support | h2 : () } msg
h2 =
    Debug.todo "h2"


h3 : Property { support | h3 : () } msg
h3 =
    Debug.todo "h3"


h4 : Property { support | h4 : () } msg
h4 =
    Debug.todo "h4"


h5 : Property { support | h5 : () } msg
h5 =
    Debug.todo "h5"


h6 : Property { support | h6 : () } msg
h6 =
    Debug.todo "h6"


footer : Property { support | footer : () } msg
footer =
    Debug.todo "footer"


aside : Property { support | aside : () } msg
aside =
    Debug.todo "aside"


label : String -> Property { support | label : () } msg
label txt =
    Debug.todo "label"


announce : Bool -> Property { support | announce : () } msg
announce urgently =
    Debug.todo "announce"



-- R E N D E R I N G


noStaticStyleSheet : Property { support | noStaticStyleSheet : () } msg
noStaticStyleSheet =
    Debug.todo "noStaticStyleSheet"


forceHover : Property { support | forceHover : () } msg
forceHover =
    Debug.todo "forceHover"


focusStyle : List (Property {} msg) -> Property { support | focusStyle : () } msg
focusStyle =
    Debug.todo "focusStyle"


layout : List (Common { noStaticStyleSheet : (), forceHover : (), focusStyle : () } msg) -> Element msg -> Html msg
layout attributes element =
    Internal.render (List.map unwrapAttribute attributes) element


html : Html msg -> Element msg
html =
    Debug.todo "html"


attribute : Attribute msg -> Property support msg
attribute =
    Debug.todo "attribute"


map : (a -> b) -> Element a -> Element b
map =
    Debug.todo "map"


mapProperty : (a -> b) -> Property support a -> Property support b
mapProperty =
    Debug.todo "mapProperty"
