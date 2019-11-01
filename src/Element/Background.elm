module Element.Background exposing
    ( color, gradient
    , image, uncropped, tiled, tiledX, tiledY
    )

{-|

@docs color, gradient


# Images

@docs image, uncropped, tiled, tiledX, tiledY

**Note** if you want more control over a background image than is provided here, you should try just using a normal `Element.image` with something like `Element.behindContent`.

-}

import Element exposing (Attribute, Color)
import Internal.Flag as Flag
import Internal.Model as Internal
import VirtualDom


{-| -}
color : Color -> Attribute { support | backgroundColor : () } msg
color clr =
    Internal.StyleClass Flag.bgColor (Internal.Colored ("bg-" ++ Internal.formatColorClass clr) "background-color" clr)


{-| Resize the image to fit the containing element while maintaining proportions and cropping the overflow.
-}
image : String -> Attribute { support | backgroundImage : () } msg
image src =
    Internal.Attr (VirtualDom.style "background" ("url(\"" ++ src ++ "\") center / cover no-repeat"))


{-| A centered background image that keeps its natural proportions, but scales to fit the space.
-}
uncropped : String -> Attribute { support | backgroundUncropped : () } msg
uncropped src =
    Internal.Attr (VirtualDom.style "background" ("url(\"" ++ src ++ "\") center / contain no-repeat"))


{-| Tile an image in the x and y axes.
-}
tiled : String -> Attribute { support | backgroundTiled : () } msg
tiled src =
    Internal.Attr (VirtualDom.style "background" ("url(\"" ++ src ++ "\") repeat"))


{-| Tile an image in the x axis.
-}
tiledX : String -> Attribute { support | backgroundTiledX : () } msg
tiledX src =
    Internal.Attr (VirtualDom.style "background" ("url(\"" ++ src ++ "\") repeat-x"))


{-| Tile an image in the y axis.
-}
tiledY : String -> Attribute { support | backgroundTiledY : () } msg
tiledY src =
    Internal.Attr (VirtualDom.style "background" ("url(\"" ++ src ++ "\") repeat-y"))


{-| A linear gradient.

First you need to specify what direction the gradient is going by providing an angle in radians. `0` is up and `pi` is down.

The colors will be evenly spaced.

-}
gradient : Float -> List Color -> Attribute { support | backgroundGradient : () } msg
gradient angle steps =
    case steps of
        [] ->
            Internal.NoAttribute

        clr :: [] ->
            Internal.StyleClass Flag.bgColor
                (Internal.Colored ("bg-" ++ Internal.formatColorClass clr) "background-color" clr)

        _ ->
            Internal.StyleClass Flag.bgGradient <|
                Internal.Single ("bg-grad-" ++ (String.join "-" <| Internal.floatClass angle :: List.map Internal.formatColorClass steps))
                    "background-image"
                    ("linear-gradient(" ++ (String.join ", " <| (String.fromFloat angle ++ "rad") :: List.map Internal.formatColor steps) ++ ")")
