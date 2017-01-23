module OnKeyDown exposing (onKeyDown, onKeyDowns)

import Html.Events exposing (on, onWithOptions)
import Json.Decode exposing (Decoder, oneOf, field, int, map, andThen, succeed, fail)


type alias KeyCode =
    Int



-- Decoders


keyCode : Decoder KeyCode
keyCode =
    oneOf
        [ field "which" int
          -- Firefox support
        , field "keyCode" int
        ]


onlyKeyCodes : List KeyCode -> Decoder KeyCode
onlyKeyCodes keyCodes =
    keyCode
        |> andThen
            (\keyCode ->
                if List.member keyCode keyCodes then
                    succeed keyCode
                else
                    fail "KeyCode not handled"
            )


onKeyDown tagger =
    on "keydown" (map tagger keyCode)


onKeyDowns keyCodes tagger =
    onWithOptions "keydown"
        { stopPropagation = True, preventDefault = True }
        (map tagger <| onlyKeyCodes keyCodes)
