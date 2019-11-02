module Internal.Box exposing (Box, map, toClass, toCss)


type alias Box x =
    { top : x
    , right : x
    , bottom : x
    , left : x
    }


map : (a -> b) -> Box a -> Box b
map tagger { top, right, bottom, left } =
    Box (tagger top) (tagger right) (tagger bottom) (tagger left)


unify : Box x -> List x
unify { top, right, bottom, left } =
    if left /= right then
        [ top, right, bottom, left ]

    else if bottom /= top then
        [ top, right, bottom ]

    else if right /= top then
        [ top, right ]

    else
        [ top ]


toClass : String -> (x -> String) -> Box x -> String
toClass prefix stringifier box =
    String.join "-" (prefix :: List.map stringifier (unify box))


toCss : String -> (x -> String) -> Box x -> String
toCss rule stringifier box =
    String.concat (rule :: ":" :: List.map ((++) " " << stringifier) (unify box)) ++ ";"
