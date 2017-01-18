module SearchableMenu
    exposing
        ( view
        , update
        , Model
        , Msg
        , openMsg
        , closeMsg
        , selectMsg
        , UpdateConfig
        , ViewConfig
        , HtmlDetails
        , SearchResult
        , MatchedString
        , viewConfig
        , updateConfig
        , initialModel
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
@docs viewConfig, updateConfig

# Model
@docs Model, initialModel

# Definitions
@docs Msg, openMsg, closeMsg, selectMsg, UpdateConfig, ViewConfig, HtmlDetails, MatchedString, SearchResult

# Helper Functions
@docs simpleSpanView, viewConfigWithClasses

-}

import SearchableMenu.SearchableMenu as Internal
import SearchableMenu.Search as Search
import Html exposing (Html, Attribute, a)
import Html.Attributes exposing (class, href, id, placeholder)
import Html.Events exposing (onClick, onBlur)


{-| Tracks the input field and the state of the menu (is it open? is the mouse over it? is there something selected?)
-}
type Model
    = Model Internal.Model


{-| A closed menu with no text in the input and nothing selected.
-}
initialModel : Model
initialModel =
    Model Internal.initialModel


{-| A message type for the menu to update.
-}
type Msg
    = Msg Internal.Msg


{-| A message to open the menu.
-}
openMsg : Msg
openMsg =
    Msg Internal.Open


{-| A message to close the menu.
-}
closeMsg : Msg
closeMsg =
    Msg Internal.Close


{-| A message to select an entry.
-}
selectMsg : String -> Msg
selectMsg =
    Msg << Internal.Select


{-| Configuration for updates
-}
type UpdateConfig msg data
    = UpdateConfig (Internal.UpdateConfig msg data)


{-| Use this function to update the model.
Provide the same data as your view.
-}
update : UpdateConfig msg data -> Msg -> Model -> List data -> ( Model, Cmd Msg, Maybe msg )
update (UpdateConfig config) (Msg msg) (Model model) data =
    let
        ( newModel, menuCommands, maybeMsg ) =
            Internal.update config msg model data
    in
        ( Model newModel, Cmd.map Msg menuCommands, maybeMsg )


{-| Create the configuration for your `update` function (`UpdateConfig`).
You provide the following information in your menu configuration:
  - `toId` &mdash; convert the data to a unique ID.
  - `textboxId` &mdash; The id attribute of the input textbox. **MUST CORRESPOND TO AN ID PROVIDED IN THE VIEW CONFIG**
  - `onSelectMsg` &mdash; The message to produce when an option is selected. It must accept and ID (String).
-}
updateConfig :
    { toId : data -> String
    , textboxId : String
    , onSelectMsg : String -> msg
    }
    -> UpdateConfig msg data
updateConfig config =
    UpdateConfig config


{-|
Configuration for your menu view.
**Note:** Your `ViewConfig` should never be held in your model. It should only appear in view code.
-}
type ViewConfig data
    = ViewConfig (Internal.ViewConfig data)


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
view : ViewConfig data -> Model -> List data -> Html Msg
view (ViewConfig config) (Model model) data =
    Html.map Msg <| Internal.view config model data


{-|
HTML lists require `li` tags as children, so we allow you to specify everything about `li` HTML node except the nodeType.
Copied from [elm-autocomplete](https://github.com/thebritican/elm-autocomplete).
-}
type alias HtmlDetails msg =
    Internal.HtmlDetails msg


type alias IsOpen =
    Bool


type alias IsSelected =
    Bool


{-| Create the configuration for your `view` function (`ViewConfig`).
You provide the following information in your menu configuration:
  - `toId` &mdash; convert the data to a unique ID.
  - `div` &mdash; a function that provides a list of `Html.Attribute Never` based on whether the menu isOpen.
  - `ul` &mdash; the attributes of the list itself.
  - `li` &mdash; a function to provide HtmlDetails for a li node, which is provided with wether the item is selected, and the SearchResult data. We provide a helper function to deal with the SearchResult data, `simpleSpanView`.
  - `input` &mdash; the attributes of the input field.
  - `prepend` &mdash; HtmlDetails for the very first li in the ul.
  - `append` &mdash; HtmlDetails for the very last li in the ul..
-}
viewConfig :
    { toId : data -> String
    , div : IsOpen -> List (Attribute Never)
    , ul : List (Attribute Never)
    , li : IsSelected -> SearchResult data -> HtmlDetails Msg
    , input : List (Attribute Never)
    , prepend : Maybe (HtmlDetails Msg)
    , append : Maybe (HtmlDetails Msg)
    }
    -> ViewConfig data
viewConfig config =
    let
        unboxMsg (Msg msg) =
            msg

        mapHtmlDetails { attributes, children } =
            { attributes = List.map (Html.Attributes.map unboxMsg) attributes
            , children = List.map (Html.map unboxMsg) children
            }

        li isSelected result =
            mapHtmlDetails <| config.li isSelected result

        prepend =
            Maybe.map mapHtmlDetails config.prepend

        append =
            Maybe.map mapHtmlDetails config.append
    in
        ViewConfig { config | li = li, prepend = prepend, append = append }


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


{-| Given a `SearchResult`, it takes the `MatchedString` and for each unmatched substring, it creates an `Html.text` node,
and for the matched substrings, it creates a `Html.span` node with the given `Attribute`s.
-}
simpleSpanView : List (Html.Attribute Never) -> SearchResult data -> List (Html Msg)
simpleSpanView attributes result =
    List.map (Html.map Msg) <| Internal.simpleSpanView attributes result


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
                    , onClick <| Msg <| Internal.Select (toId << Tuple.second <| result)
                    , onBlur <| Msg Internal.LostFocus
                    ]
                    (simpleSpanView [ class matchedSpanClass ] result)
                ]
            }
    in
        viewConfig
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
                    Internal.HtmlDetails
                        [ if isSelected then
                            class selectedClass
                          else
                            class liClass
                        ]
                        [ a [ href "#0", onClick <| Msg <| Internal.Select (toId << Tuple.second <| result), onBlur <| Msg Internal.LostFocus ] (simpleSpanView [ class matchedSpanClass ] result) ]
                )
            , input = [ class inputClass, id inputId, placeholder inputPlaceholder ]
            , prepend = Nothing
            , append = Nothing
            }
