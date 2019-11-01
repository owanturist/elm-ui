module Element.Border exposing
    ( color
    , width, widthXY, widthEach
    , solid, dashed, dotted
    , rounded, roundEach
    , glow, innerGlow, shadow, innerShadow
    )

{-|

@docs color


## Border Widths

@docs width, widthXY, widthEach


## Border Styles

@docs solid, dashed, dotted


## Rounded Corners

@docs rounded, roundEach


## Shadows

@docs glow, innerGlow, shadow, innerShadow

-}

import Element exposing (Attribute, Color)
import Internal.Flag as Flag
import Internal.Model as Internal
import Internal.Style exposing (classes)


{-| -}
color : Color -> Attribute { support | borderColor : () } msg
color clr =
    Internal.StyleClass
        Flag.borderColor
        (Internal.Colored
            ("bc-" ++ Internal.formatColorClass clr)
            "border-color"
            clr
        )


widthHelp : Int -> Int -> Int -> Int -> Attribute support msg
widthHelp top right bottom left =
    let
        fragments =
            if right /= left then
                [ top, right, bottom, left ]

            else if top /= bottom then
                [ top, right, bottom ]

            else if top /= right then
                [ top, right ]

            else
                [ top ]
    in
    Internal.BorderWidth
        (String.concat ("b-" :: List.map String.fromInt fragments))
        top
        right
        bottom
        left
        |> Internal.StyleClass Flag.borderWidth


{-| -}
width : Int -> Attribute { support | borderWidth : () } msg
width v =
    widthHelp v v v v


{-| Set horizontal and vertical borders.
-}
widthXY : Int -> Int -> Attribute { support | borderWidthXY : () } msg
widthXY x y =
    widthXY y x


{-| -}
widthEach :
    { bottom : Int
    , left : Int
    , right : Int
    , top : Int
    }
    -> Attribute { support | borderWidthEach : () } msg
widthEach { top, right, bottom, left } =
    widthHelp top right bottom left


{-| -}
solid : Attribute { support | borderSolid : () } msg
solid =
    Internal.Class Flag.borderStyle classes.borderSolid


{-| -}
dashed : Attribute { support | borderDashed : () } msg
dashed =
    Internal.Class Flag.borderStyle classes.borderDashed


{-| -}
dotted : Attribute { support | borderDotted : () } msg
dotted =
    Internal.Class Flag.borderStyle classes.borderDotted


{-| Round all corners.
-}
rounded : Int -> Attribute { support | borderRounded : () } msg
rounded radius =
    Internal.StyleClass
        Flag.borderRound
        (Internal.Single
            ("br-" ++ String.fromInt radius)
            "border-radius"
            (String.fromInt radius ++ "px")
        )


{-| -}
roundEach :
    { topLeft : Int
    , topRight : Int
    , bottomLeft : Int
    , bottomRight : Int
    }
    -> Attribute { support | borderRoundEach : () } msg
roundEach { topLeft, topRight, bottomLeft, bottomRight } =
    Internal.StyleClass Flag.borderRound
        (Internal.Single
            ("br-"
                ++ String.fromInt topLeft
                ++ "-"
                ++ String.fromInt topRight
                ++ String.fromInt bottomLeft
                ++ "-"
                ++ String.fromInt bottomRight
            )
            "border-radius"
            (String.fromInt topLeft
                ++ "px "
                ++ String.fromInt topRight
                ++ "px "
                ++ String.fromInt bottomRight
                ++ "px "
                ++ String.fromInt bottomLeft
                ++ "px"
            )
        )


shadowHelp :
    Bool
    ->
        { offset : ( Float, Float )
        , size : Float
        , blur : Float
        , color : Color
        }
    -> Attribute support msg
shadowHelp inset almostShade =
    let
        shade =
            { inset = inset
            , offset = almostShade.offset
            , size = almostShade.size
            , blur = almostShade.blur
            , color = almostShade.color
            }
    in
    Internal.StyleClass Flag.shadows <|
        Internal.Single
            (Internal.boxShadowClass shade)
            "box-shadow"
            (Internal.formatBoxShadow shade)


{-| A simple glow by specifying the color and size.
-}
glow : Color -> Float -> Attribute { support | borderGlow : () } msg
glow clr size =
    shadowHelp False
        { offset = ( 0, 0 )
        , size = size
        , blur = size * 2
        , color = clr
        }


{-| -}
innerGlow : Color -> Float -> Attribute { support | borderInnerGlow : () } msg
innerGlow clr size =
    shadowHelp True
        { offset = ( 0, 0 )
        , size = size
        , blur = size * 2
        , color = clr
        }


{-| -}
shadow :
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Color
    }
    -> Attribute { support | borderShadow : () } msg
shadow =
    shadowHelp False


{-| -}
innerShadow :
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Color
    }
    -> Attribute { support | borderInnerShadow : () } msg
innerShadow =
    shadowHelp True
