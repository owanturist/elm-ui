module Internal.Color exposing (Color(..), toClass, toCss)


type Color
    = Rgba Int Int Int Int


toClass : String -> Color -> String
toClass prefix (Rgba r g b a) =
    String.join "-" (prefix :: List.map String.fromInt [ r, g, b, a ])


toCss : String -> Color -> String
toCss rule (Rgba r g b a) =
    rule ++ ":rgba(" ++ String.join "," (List.map String.fromInt [ r, g, b, a ]) ++ ");"
