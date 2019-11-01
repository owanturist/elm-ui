module Element.Region exposing
    ( mainContent, navigation, heading, aside, footer
    , description
    , announce, announceUrgently
    )

{-| This module is meant to make accessibility easy!

These are sign posts that accessibility software like screen readers can use to navigate your app.

All you have to do is add them to elements in your app where you see fit.

Here's an example of annotating your navigation region:

    import Element.Region as Region

    myNavigation =
        Element.row [ Region.navigation ]
            [-- ..your navigation links
            ]

@docs mainContent, navigation, heading, aside, footer

@docs description

@docs announce, announceUrgently

-}

import Element exposing (Attribute)
import Internal.Model as Internal exposing (Description(..))


{-| -}
mainContent : Attribute { support | mainContent : () } msg
mainContent =
    Internal.Describe Main


{-| -}
aside : Attribute { support | aside : () } msg
aside =
    Internal.Describe Complementary


{-| -}
navigation : Attribute { support | navigation : () } msg
navigation =
    Internal.Describe Navigation


{-| -}
footer : Attribute { support | footer : () } msg
footer =
    Internal.Describe ContentInfo


{-| This will mark an element as `h1`, `h2`, etc where possible.

Though it's also smart enough to not conflict with existing nodes.

So, this code

    link [ Region.heading 1 ]
        { url = "http://fruits.com"
        , label = text "Best site ever"
        }

will generate

    <a href="http://fruits.com">
        <h1>Best site ever</h1>
    </a>

-}
heading : Int -> Attribute { support | heading : () } msg
heading =
    Internal.Describe << Heading


{-| Screen readers will announce changes to this element and potentially interrupt any other announcement.
-}
announceUrgently : Attribute { support | announceUrgently : () } msg
announceUrgently =
    Internal.Describe LiveAssertive


{-| Screen readers will announce when changes to this element are made.
-}
announce : Attribute { support | announce : () } msg
announce =
    Internal.Describe LivePolite


{-| -}
description : String -> Attribute { support | description : () } msg
description =
    Internal.Describe << Internal.Label
