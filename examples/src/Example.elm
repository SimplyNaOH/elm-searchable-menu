module Example exposing (..)

import Html exposing (Html, body, div, h2, p, text, ul, li, a)
import Html.Attributes exposing (class, href, style, id, placeholder)
import Html.Events exposing (onClick)
import SearchableMenu


-- Model


type alias Topic =
    { name : String
    , color : String
    }


type alias Model =
    { topics : List Topic
    , otherTopics : List Topic
    , topicMenu : SearchableMenu.Model
    , advancedMenu : SearchableMenu.Model
    }


initialModel =
    Model [] [] SearchableMenu.initialModel SearchableMenu.initialModel



-- Update


type Msg
    = ToggleTopic String
    | ToggleOtherTopic String
    | TopicMenuMsg SearchableMenu.Msg
    | AdvancedMenuMsg SearchableMenu.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noCmd model =
            ( model, Cmd.none )

        toggle name list =
            if List.isEmpty (List.filter ((==) name << .name) list) then
                list ++ List.filter ((==) name << .name) exampleTopics
            else
                List.filter ((/=) name << .name) list

        attachCmd ( model, oldCmd ) newCmd =
            model ! [ oldCmd, newCmd ]
    in
        case msg of
            ToggleTopic name ->
                noCmd { model | topics = toggle name model.topics }

            ToggleOtherTopic name ->
                noCmd { model | otherTopics = toggle name model.otherTopics }

            TopicMenuMsg msg ->
                let
                    config =
                        SearchableMenu.updateConfig
                            { toId = .name
                            , textboxId = "topic-menu__input"
                            , onSelectMsg = ToggleTopic
                            }

                    ( updatedMenu, menuCmd, maybeMsg ) =
                        SearchableMenu.update config msg model.topicMenu exampleTopics
                in
                    case maybeMsg of
                        Nothing ->
                            ( { model | topicMenu = updatedMenu }, Cmd.map TopicMenuMsg menuCmd )

                        Just msg ->
                            attachCmd (update msg model) (Cmd.map TopicMenuMsg menuCmd)

            AdvancedMenuMsg msg ->
                let
                    config =
                        SearchableMenu.updateConfig
                            { toId = .name
                            , textboxId = "topic-menu__input2"
                            , onSelectMsg = ToggleOtherTopic
                            }

                    ( updatedMenu, menuCmd, maybeMsg ) =
                        SearchableMenu.update config msg model.advancedMenu exampleTopics
                in
                    case maybeMsg of
                        Nothing ->
                            ( { model | advancedMenu = updatedMenu }, Cmd.map AdvancedMenuMsg menuCmd )

                        Just msg ->
                            attachCmd (update msg model) (Cmd.map AdvancedMenuMsg menuCmd)

            NoOp ->
                noCmd model



-- View


topicView : (String -> Msg) -> Topic -> Html Msg
topicView msg topic =
    li [ class "topic-container__topic" ]
        [ a
            [ href "#0"
            , onClick <| msg topic.name
            , style [ ( "color", topic.color ) ]
            ]
            [ text topic.name ]
        ]


topicMenuViewConfig =
    SearchableMenu.viewConfigWithClasses
        { toId = .name
        , openClass = "topic-menu"
        , closedClass = "topic-menu topic-menu--closed"
        , ulClass = "topic-menu__list"
        , liClass = "topic-menu__topic"
        , selectedClass = "topic-menu__topic topic-menu__topic--selected"
        , matchedSpanClass = "topic-menu__matched-string"
        , inputClass = "topic-menu__input"
        , inputId = "topic-menu__input"
        , inputPlaceholder = "Search topics"
        }


advancedMenuTopicView model isSelected result =
    let
        ( _, topic ) =
            result

        aStyle =
            style <|
                ( "color", topic.color )
                    :: if List.member topic model.otherTopics then
                        [ ( "background-color", "#888" ) ]
                       else
                        []
    in
        { attributes =
            [ class <|
                if isSelected then
                    "topic-menu__topic topic-menu__topic--selected"
                else
                    "topic-menu__topic"
            ]
        , children =
            [ a [ href "#0", onClick <| SearchableMenu.selectMsg topic.name, aStyle ]
                (SearchableMenu.simpleSpanView [ class "topic-menu__matched-string" ] result)
            ]
        }


advancedMenuViewConfig model =
    SearchableMenu.viewConfig
        { toId = .name
        , div =
            \isOpen ->
                [ class <|
                    if isOpen then
                        "advanced-menu"
                    else
                        "advanced-menu advanced-menu--closed"
                ]
        , ul = [ class "advanced-menu__list" ]
        , li = advancedMenuTopicView model
        , input =
            [ id "topic-menu__input2"
            , class "advanced-menu__input"
            , placeholder "Add a topic"
            ]
        , prepend = Nothing
        , append =
            Just <|
                a
                    [ href "#0"
                    , class "advanced-menu__close-button"
                    , onClick <| SearchableMenu.closeMsg
                    ]
                    [ text "Close" ]
        }


openButton menuMsg =
    a
        [ href "#0"
        , class "topic-container__open-button"
        , onClick <| menuMsg SearchableMenu.openMsg
        ]
        [ text "Add a topic" ]


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Select some topics" ]
        , ul [ class "topic-container" ] <|
            List.map (topicView ToggleTopic) model.topics
        , openButton TopicMenuMsg
        , Html.map TopicMenuMsg <| SearchableMenu.view topicMenuViewConfig model.topicMenu exampleTopics
        , h2 [] [ text "Select some other topics" ]
        , ul [ class "topic-container" ] <|
            List.map (topicView ToggleOtherTopic) model.otherTopics
        , Html.map AdvancedMenuMsg <| SearchableMenu.view (advancedMenuViewConfig model) model.advancedMenu exampleTopics
        ]



-- Entry point


main =
    Html.program { init = ( initialModel, Cmd.none ), view = view, update = update, subscriptions = always Sub.none }



-- Topics
-- Extracted from https://en.wiktionary.org/wiki/Category:en:Computer_science


exampleTopics : List Topic
exampleTopics =
    [ Topic "access token" "rgb(140,225,73)"
    , Topic "accumulator" "rgb(125,40,187)"
    , Topic "ad-hoc polymorphism" "rgb(35,185,243)"
    , Topic "add" "rgb(162,192,219)"
    , Topic "adicity" "rgb(147,207,80)"
    , Topic "algorithm" "rgb(105,173,217)"
    , Topic "alphabet" "rgb(182,201,176)"
    , Topic "amortize" "rgb(38,7,250)"
    , Topic "anamorphism" "rgb(4,100,67)"
    , Topic "arity" "rgb(238,41,135)"
    , Topic "assert" "rgb(24,205,224)"
    , Topic "bag of words" "rgb(115,12,175)"
    , Topic "BC" "rgb(56,24,117)"
    , Topic "beta reduce" "rgb(93,198,97)"
    , Topic "bifurcation" "rgb(235,87,153)"
    , Topic "bioinformatics" "rgb(55,93,236)"
    , Topic "bisimilar" "rgb(148,37,134)"
    , Topic "brute force" "rgb(171,213,236)"
    , Topic "buffer overflow" "rgb(211,146,126)"
    , Topic "built-in type" "rgb(200,210,153)"
    , Topic "burstiness" "rgb(143,201,157)"
    , Topic "bursty" "rgb(30,227,216)"
    , Topic "cannot-link" "rgb(204,113,222)"
    , Topic "case dependent" "rgb(60,195,135)"
    , Topic "case insensitive" "rgb(202,228,184)"
    , Topic "case sensitive" "rgb(170,43,136)"
    , Topic "case-dependent" "rgb(179,131,63)"
    , Topic "case-sensitive" "rgb(100,71,90)"
    , Topic "catamorphism" "rgb(246,92,148)"
    , Topic "chatbot" "rgb(222,235,212)"
    , Topic "chunklet" "rgb(101,167,128)"
    , Topic "codebook" "rgb(198,191,165)"
    , Topic "combinator" "rgb(147,157,173)"
    , Topic "combinatory logic" "rgb(9,134,155)"
    , Topic "complexity theory" "rgb(228,16,36)"
    , Topic "composite type" "rgb(97,146,240)"
    , Topic "computability theory" "rgb(115,58,107)"
    , Topic "computable function" "rgb(157,99,141)"
    , Topic "computation history" "rgb(175,71,101)"
    , Topic "computation tree logic" "rgb(132,13,228)"
    , Topic "computational linguistics" "rgb(16,211,54)"
    , Topic "computer engineering" "rgb(9,255,200)"
    , Topic "computer science" "rgb(124,83,14)"
    , Topic "concave function" "rgb(10,18,196)"
    , Topic "concurrency" "rgb(177,115,143)"
    , Topic "constraint cluster" "rgb(47,127,2)"
    , Topic "convex function" "rgb(78,226,246)"
    , Topic "crayon license" "rgb(80,25,49)"
    , Topic "CRF" "rgb(83,73,243)"
    , Topic "data structure" "rgb(31,130,176)"
    , Topic "debug" "rgb(162,63,97)"
    , Topic "decidability" "rgb(155,184,203)"
    , Topic "decidable" "rgb(36,243,158)"
    , Topic "declarer" "rgb(28,235,53)"
    , Topic "decompile" "rgb(88,4,87)"
    , Topic "decompiler" "rgb(57,199,88)"
    , Topic "deconflict" "rgb(110,213,209)"
    , Topic "delimiter" "rgb(253,111,40)"
    ]
