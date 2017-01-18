module SearchableMenu.Search exposing (SearchResult, MatchedString, search, simpleSpanView)

import Html exposing (Html, text, span)


type alias MatchedString =
    List ( Bool, String )


type alias SearchResult a =
    ( MatchedString, a )


decomposeMatch : String -> String -> MatchedString
decomposeMatch str source =
    let
        indices =
            String.indices (String.toLower str) source
    in
        case indices of
            [] ->
                [ ( False, source ) ]

            x :: _ ->
                [ ( False, String.slice 0 x source )
                , ( True, String.slice x (x + String.length str) source )
                , ( False, String.dropLeft (x + String.length str) source )
                ]


search : String -> (a -> String) -> List a -> List (SearchResult a)
search str f elems =
    let
        readyToMatch =
            List.map (\elem -> ( String.toLower <| f elem, elem )) elems

        matches =
            List.filter (\( toMatch, _ ) -> String.contains (String.toLower str) toMatch) readyToMatch
    in
        if String.isEmpty str then
            List.map (\elem -> ( [ ( False, f elem ) ], elem )) elems
        else
            List.map (\( matched, elem ) -> ( decomposeMatch str matched, elem )) matches


simpleSpanView : List (Html.Attribute Never) -> SearchResult a -> List (Html Never)
simpleSpanView spanAttributes ( matches, _ ) =
    let
        matchedView ( isMatch, str ) =
            if isMatch then
                span spanAttributes [ text str ]
            else
                text str
    in
        List.map matchedView matches
