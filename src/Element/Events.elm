module Element.Events exposing
    ( onClick, onDoubleClick, onMouseDown, onMouseUp, onMouseEnter, onMouseLeave, onMouseMove
    , onFocus, onLoseFocus
    , on
    )

{-|


## Mouse Events

@docs onClick, onDoubleClick, onMouseDown, onMouseUp, onMouseEnter, onMouseLeave, onMouseMove


## Focus Events

@docs onFocus, onLoseFocus


## Custom Events

@docs on

-}

import Element exposing (Attribute)
import Html.Events
import Internal.Model as Internal
import Json.Decode as Json



-- MOUSE EVENTS


{-| -}
onMouseDown : msg -> Attribute { support | onMouseDown : () } msg
onMouseDown =
    Internal.Attr << Html.Events.onMouseDown


{-| -}
onMouseUp : msg -> Attribute { support | onMouseUp : () } msg
onMouseUp =
    Internal.Attr << Html.Events.onMouseUp


{-| -}
onClick : msg -> Attribute { support | onClick : () } msg
onClick =
    Internal.Attr << Html.Events.onClick


{-| -}
onDoubleClick : msg -> Attribute { support | onDoubleClick : () } msg
onDoubleClick =
    Internal.Attr << Html.Events.onDoubleClick


{-| -}
onMouseEnter : msg -> Attribute { support | onMouseEnter : () } msg
onMouseEnter =
    Internal.Attr << Html.Events.onMouseEnter


{-| -}
onMouseLeave : msg -> Attribute { support | onMouseLeave : () } msg
onMouseLeave =
    Internal.Attr << Html.Events.onMouseLeave


{-| -}
onMouseMove : msg -> Attribute { support | onMouseMove : () } msg
onMouseMove msg =
    onHelp "mousemove" (Json.succeed msg)



-- FOCUS EVENTS


{-| -}
onLoseFocus : msg -> Attribute { support | onLoseFocus : () } msg
onLoseFocus =
    Internal.Attr << Html.Events.onBlur


{-| -}
onFocus : msg -> Attribute { support | onFocus : () } msg
onFocus =
    Internal.Attr << Html.Events.onFocus



-- CUSTOM EVENTS


onHelp : String -> Json.Decoder msg -> Attribute support msg
onHelp event decode =
    Internal.Attr <| Html.Events.on event decode


{-| Create a custom event listener. Normally this will not be necessary, but
you have the power! Here is how `onClick` is defined for example:

    import Json.Decode as Json

    onClick : msg -> Attribute msg
    onClick message =
        on "click" (Json.succeed message)

The first argument is the event name in the same format as with JavaScript's
[`addEventListener`][aEL] function.
The second argument is a JSON decoder. Read more about these [here][decoder].
When an event occurs, the decoder tries to turn the event object into an Elm
value. If successful, the value is routed to your `update` function. In the
case of `onClick` we always just succeed with the given `message`.
If this is confusing, work through the [Elm Architecture Tutorial][tutorial].
It really does help!
[aEL]: <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener>
[decoder]: <http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode>
[tutorial]: <https://github.com/evancz/elm-architecture-tutorial/>

-}
on : String -> Json.Decoder msg -> Attribute { support | on : () } msg
on =
    onHelp
