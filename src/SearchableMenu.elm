module SearchableMenu
    exposing
        ( view
        , update
        , initialModel
        , Model
        , Msg(..)
        , UpdateConfig
        , ViewConfig
        , HtmlDetails
        , SearchResult
        , MatchedString
        , simpleSpanView
        , viewConfigWithClasses
        )

{-|
 This library helps you create a searchable menu.
 It is heavily inspired in [elm-autocomplete](https://github.com/thebritican/elm-autocomplete), so I recommend checking their implementation first to see if it suits your needs.

 While elm-autocomplete provides a menu that offers suggestions to the user based on some input, this menu is focused on providing a complete list of possible selections, while also being able to use an input field to search through the options.

 The project is still very raw and I wuldn't recommend using it without first reading the imlementation.

# View
@docs view

# Update
@docs update

# Configuration
@docs ViewConfig, UpdateConfig

# Model
@docs Model, initialModel

# Definitions
@docs Msg, HtmlDetails, MatchedString, SearchResult

# Helper Functions
@docs simpleSpanView, viewConfigWithClasses

-}

-- Html stuff

import Html exposing (Html, Attribute, div, input, li, text, span, a)
import Html.Keyed exposing (ul)
import Html.Attributes exposing (id, class, placeholder, value, href)
import Html.Events exposing (onInput, onBlur, onFocus, onMouseLeave, onMouseEnter, onClick)
import OnKeyDown exposing (onKeyDown, onKeyDowns)


-- Other things

import Task
import Dom exposing (blur)
import Search as Search
import Search exposing (SearchResult, search)


-- Model


{-| Tracks the input field and the state of the menu (is it open? is the mouse over it? is there something selected?)
-}
type alias Model =
    { searchString : String
    , mouseIsOver : Bool
    , menuIsOpen : Bool
    , selected : Maybe Int
    }


{-| A closed menu with no text in the input and nothing selected.
-}
initialModel : Model
initialModel =
    Model "" False False Nothing



-- Update


type alias KeyCode =
    Int


{-| A message type for the menu to update.
-}
type Msg
    = Search String
    | Open
    | Close
    | SetMouseOver Bool
    | LostFocus
    | KeyDown KeyCode
    | Select String
    | NoOp


{-| Configuration for updates
You provide the following information in your menu configuration:
  - `toId` &mdash; convert the data to a unique ID.
  - `textboxId` &mdash; The id attribute of the input textbox. **MUST CORRESPOND TO AN ID PROVIDED IN THE VIEW CONFIG**
  - `onSelectMsg` &mdash; The message to produce when an option is selected. It must accept and ID (String).
-}
type alias UpdateConfig msg a =
    { toId : a -> String
    , textboxId : String
    , onSelectMsg : String -> msg
    }


{-| Use this function to update the model.
Provide the same data as your view.
-}
update : UpdateConfig msg a -> Msg -> Model -> List a -> ( Model, Cmd Msg, Maybe msg )
update config msg model data =
    let
        noCmdMsg model =
            ( model, Cmd.none, Nothing )
    in
        case msg of
            Search str ->
                noCmdMsg
                    { model
                        | searchString = str
                        , selected =
                            if String.isEmpty str then
                                Nothing
                            else
                                Just 0
                    }

            Open ->
                ( { model | menuIsOpen = True }
                , Task.attempt (always NoOp) (Dom.focus config.textboxId)
                , Nothing
                )

            Close ->
                noCmdMsg { model | searchString = "", selected = Nothing, menuIsOpen = False }

            SetMouseOver isOver ->
                noCmdMsg { model | mouseIsOver = isOver }

            LostFocus ->
                if model.mouseIsOver then
                    noCmdMsg { model | selected = Nothing }
                    -- right now we can't handle keydown events outside the input, so we don't want a selection
                else
                    noCmdMsg initialModel

            KeyDown keyCode ->
                let
                    maxSelect =
                        List.length (search model.searchString config.toId data) - 1

                    selectNext =
                        Maybe.withDefault 0 <|
                            Maybe.map (min maxSelect << (+) 1) model.selected

                    selectPrev =
                        Maybe.withDefault maxSelect <|
                            Maybe.map (max 0 << flip (-) 1) model.selected

                    at list index =
                        List.head << List.drop index <| list

                    searchResults =
                        search model.searchString config.toId data

                    selectedId =
                        Maybe.andThen
                            (Maybe.map (config.toId << Tuple.second) << at searchResults)
                            model.selected
                in
                    case keyCode of
                        38 ->
                            -- up arrow
                            noCmdMsg { model | selected = Just selectPrev }

                        40 ->
                            -- down arrow
                            noCmdMsg { model | selected = Just selectNext }

                        13 ->
                            -- enter
                            Maybe.withDefault (noCmdMsg model) <|
                                Maybe.map (\id -> update config (Select id) model data) selectedId

                        27 ->
                            -- esc
                            ( initialModel
                            , Task.attempt (always NoOp) (Dom.blur config.textboxId)
                            , Nothing
                            )

                        _ ->
                            noCmdMsg model

            Select id ->
                ( { model | searchString = "", selected = Nothing }
                , Cmd.none
                , Just <| config.onSelectMsg id
                )

            NoOp ->
                noCmdMsg model



-- View


type alias IsSelected =
    Bool


type alias IsOpen =
    Bool


{-|
HTML lists require `li` tags as children, so we allow you to specify everything about `li` HTML node except the nodeType.
Copied from [elm-autocomplete](https://github.com/thebritican/elm-autocomplete).
-}
type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-|
Configuration for your menu view.
**Note:** Your `ViewConfig` should never be held in your model. It should only appear in view code.
You provide the following information in your menu configuration:
  - `toId` &mdash; convert the data to a unique ID.
  - `div` &mdash; a function that provides a list of `Html.Attribute Never` based on whether the menu isOpen.
  - `ul` &mdash; the attributes of the list itself.
  - `li` &mdash; a function to provide HtmlDetails for a li node, which is provided with wether the item is selected, and the SearchResult data. We provide a helper function to deal with the SearchResult data, `simpleSpanView`.
  - `input` &mdash; the attributes of the input field.
  - `prepend` &mdash; HtmlDetails for the very first li in the ul.
  - `append` &mdash; HtmlDetails for the very last li in the ul..
-}
type alias ViewConfig a =
    { toId : a -> String
    , div : IsOpen -> List (Attribute Never)
    , ul : List (Attribute Never)
    , li : IsSelected -> SearchResult a -> HtmlDetails Msg
    , input : List (Attribute Never)
    , prepend : Maybe (HtmlDetails Msg)
    , append : Maybe (HtmlDetails Msg)
    }


textbox : ViewConfig a -> String -> Html Msg
textbox config text =
    let
        eventAttributes =
            [ onInput Search
            , onBlur LostFocus
            , onFocus Open
            , onKeyDown KeyDown
            , value text
            ]

        customAttributes =
            List.map mapNeverToMsg config.input
    in
        input (eventAttributes ++ customAttributes)
            []


{-|
Take a list of `data` and turn it into a searchable menu.
The `ViewConfig` argument is the configuration for the menu view.
`ViewConfig` describes the HTML we want to show for each item, the list, the input, and a container div.
**Note:** The `Model` and `List data` should live in your Model.
The `ViewConfig` for the menu belongs in your view code.
`ViewConfig` should never exist in your model.
Describe any potential menu configurations statically.
This pattern has been inspired by [Elm Sortable Table](http://package.elm-lang.org/packages/evancz/elm-sortable-table/latest).
The above statements have been copied from [elm-autocomplete](https://github.com/thebritican/elm-autocomplete).
-}
view : ViewConfig a -> Model -> List a -> Html Msg
view config model data =
    let
        mouseOnDivAttributes =
            [ onMouseEnter <| SetMouseOver True
            , onMouseLeave <| SetMouseOver False
            ]

        customDivAttributes =
            List.map mapNeverToMsg <| config.div model.menuIsOpen

        toLi htmlDetails =
            li htmlDetails.attributes
                htmlDetails.children

        isSelected i =
            Maybe.withDefault False <| Maybe.map ((==) i) model.selected

        viewAndId i item =
            ( config.toId (Tuple.second item), toLi <| config.li (isSelected i) item )

        dataList =
            List.indexedMap viewAndId <| search model.searchString config.toId data

        appendix =
            Maybe.withDefault [] (Maybe.map (\x -> [ ( "appendix", toLi x ) ]) config.append)

        list =
            case config.prepend of
                Nothing ->
                    dataList ++ appendix

                Just prepend ->
                    ( "prepend", toLi prepend ) :: dataList ++ appendix
    in
        div (mouseOnDivAttributes ++ customDivAttributes)
            [ textbox config model.searchString
            , ul (onKeyDowns [ 38, 40, 13, 27 ] KeyDown :: List.map mapNeverToMsg config.ul) list
            ]


{-|
Given a search on some data, `SearchResult data` is a tuple containing the matched search query and the data itself.
-}
type alias SearchResult data =
    Search.SearchResult data


{-|
Given a search on a string, `MatchedString` is a list of substrings, accompanied by wether they are a match for the search query.
-}
type alias MatchedString =
    Search.MatchedString



-- Helper Functions


mapNeverToMsg attr =
    Html.Attributes.map (always NoOp) attr


{-| Given a `SearchResult`, it takes the `MatchedString` and for each unmatched substring, it creates an `Html.text` node,
and for the matched substrings, it creates a `Html.span` node with the given `Attribute`s.
-}
simpleSpanView : List (Html.Attribute Never) -> SearchResult a -> List (Html Msg)
simpleSpanView attrs res =
    List.map (Html.map (always NoOp)) <| Search.simpleSpanView attrs res


{-|
Simple viewConfig providing only css classes for the different elements.
-}
viewConfigWithClasses :
    { toId : data -> String
    , openClass : String
    , closedClass : String
    , ulClass : String
    , liClass : String
    , selectedClass : String
    , matchedSpanClass : String
    , inputClass : String
    , inputId : String
    , inputPlaceholder : String
    }
    -> ViewConfig data
viewConfigWithClasses { toId, openClass, closedClass, ulClass, liClass, selectedClass, matchedSpanClass, inputClass, inputId, inputPlaceholder } =
    let
        simpleLiView : IsSelected -> SearchResult data -> HtmlDetails Msg
        simpleLiView isSelected result =
            { attributes =
                [ if isSelected then
                    class selectedClass
                  else
                    class liClass
                ]
            , children =
                [ a
                    [ href "#0"
                    , onClick <| Select (toId << Tuple.second <| result)
                    , onBlur <| LostFocus
                    ]
                    (simpleSpanView [ class matchedSpanClass ] result)
                ]
            }
    in
        { toId = toId
        , div =
            \isOpen ->
                if isOpen then
                    [ class openClass ]
                else
                    [ class closedClass ]
        , ul = [ class ulClass ]
        , li =
            (\isSelected result ->
                HtmlDetails
                    [ if isSelected then
                        class selectedClass
                      else
                        class liClass
                    ]
                    [ a [ href "#0", onClick <| Select (toId << Tuple.second <| result), onBlur LostFocus ] (simpleSpanView [ class matchedSpanClass ] result) ]
            )
        , input = [ class inputClass, id inputId, placeholder inputPlaceholder ]
        , prepend = Nothing
        , append = Nothing
        }
