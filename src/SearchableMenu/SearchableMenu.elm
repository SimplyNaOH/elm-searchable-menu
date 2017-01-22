module SearchableMenu.SearchableMenu
    exposing
        ( view
        , update
        , initialModel
        , Model
        , Msg(..)
        , UpdateConfig
        , ViewConfig
        , HtmlDetails
        , simpleSpanView
        )

-- Html stuff

import Html exposing (Html, Attribute, div, input, li, text, span, a)
import Html.Keyed exposing (ul)
import Html.Attributes exposing (id, class, placeholder, value)
import Html.Events exposing (onInput, onBlur, onFocus, onMouseLeave, onMouseEnter, onClick)
import SearchableMenu.OnKeyDown exposing (onKeyDown, onKeyDowns)


-- Other things

import Task
import Dom exposing (blur)
import SearchableMenu.Search as Search
import SearchableMenu.Search exposing (SearchResult, search)


-- Model


type alias Model =
    { searchString : String
    , mouseIsOver : Bool
    , menuIsOpen : Bool
    , selected : Maybe Int
    }


initialModel =
    Model "" False False Nothing



-- Update


type alias KeyCode =
    Int


type Msg
    = Search String
    | Open
    | Close
    | SetMouseOver Bool
    | LostFocus
    | KeyDown KeyCode
    | Select String
    | NoOp


type alias UpdateConfig msg a =
    { toId : a -> String
    , textboxId : String
    , onSelectMsg : String -> msg
    }


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


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


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


simpleSpanView : List (Html.Attribute Never) -> SearchResult a -> List (Html Msg)
simpleSpanView attrs res =
    List.map (Html.map (always NoOp)) <| Search.simpleSpanView attrs res



-- Helper Functions


mapNeverToMsg attr =
    Html.Attributes.map (always NoOp) attr
