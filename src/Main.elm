module Main exposing (Model, Msg, init, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes
import Html.Events
import Maybe.Extra
import View.Plot as Plot



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES


type alias Model =
    { data : Array Data
    , graphs : Dict String Bool
    }


type alias Data =
    { name : String
    , value : Float
    , color : String
    }


type Figure
    = Plot


type Msg
    = ToggleGraphTypeCheck String
    | AddVariableData
    | ChangeVariableDataName Int Data String
    | ChangeVariableDataValue Int Data String
    | ChangeVariableDataColor Int Data String



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = Array.fromList [ { name = "x", value = 80, color = "#ff2312" } ], graphs = Dict.fromList [ ( figureToString Plot, False ) ] }, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleGraphTypeCheck figure ->
            ( { model | graphs = toggle figure model.graphs }, Cmd.none )

        AddVariableData ->
            ( { model | data = Array.push { name = "y", value = 15, color = "#551298" } model.data }, Cmd.none )

        ChangeVariableDataName index data name ->
            ( { model | data = Array.set index { data | name = name } model.data }, Cmd.none )

        ChangeVariableDataValue index data value ->
            ( { model | data = Array.set index { data | value = Maybe.withDefault data.value (String.toFloat value) } model.data }, Cmd.none )

        ChangeVariableDataColor index data color ->
            ( { model | data = Array.set index { data | color = color } model.data }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Data Viewer"
    , body =
        [ viewHeader model
        , viewMain model
        ]
    }


viewHeader : Model -> Html.Html Msg
viewHeader model =
    Html.header []
        [ Html.ul []
            [ Html.li [] [ Html.text "Elm Data Viewer" ]
            ]
        ]


viewMain : Model -> Html.Html Msg
viewMain model =
    Html.main_ []
        [ viewDataList model
        , viewGraphTypesList model
        , viewFiguresList model
        ]


viewDataList : Model -> Html.Html Msg
viewDataList model =
    Html.section []
        [ Html.header [] [ Html.text "Values" ]
        , Html.p [] [ Html.text "Insert one or more values" ]
        , Html.div [] (List.map viewData (Array.toIndexedList model.data))
        , Html.button [ Html.Events.onClick AddVariableData ] [ Html.text "Add" ]
        ]


viewData : ( Int, Data ) -> Html.Html Msg
viewData ( index, data ) =
    Html.div []
        [ Html.input [ Html.Attributes.type_ "text", Html.Attributes.value data.name, Html.Events.onInput (\s -> ChangeVariableDataName index data s) ] []
        , Html.input [ Html.Attributes.type_ "number", Html.Attributes.value (String.fromFloat data.value), Html.Events.onInput (\s -> ChangeVariableDataValue index data s) ] []
        , Html.input [ Html.Attributes.type_ "text", Html.Attributes.value data.color, Html.Events.onInput (\s -> ChangeVariableDataColor index data s) ] []
        ]


viewGraphTypesList : Model -> Html.Html Msg
viewGraphTypesList model =
    Html.section []
        [ Html.header [] [ Html.text "Graphs" ]
        , Html.p [] [ Html.text "Select one or more graphs" ]
        , Html.div [] <| List.map viewGraphType <| Dict.keys model.graphs
        ]


viewGraphType : String -> Html.Html Msg
viewGraphType figure =
    Html.div []
        [ Html.input [ Html.Attributes.type_ "checkbox", Html.Events.onClick <| ToggleGraphTypeCheck figure ] []
        , Html.label [ Html.Attributes.for "" ] [ Html.text figure ]
        ]


viewFiguresList : Model -> Html.Html Msg
viewFiguresList model =
    Html.section []
        [ Html.header [] [ Html.text "Figures" ]
        , Html.div [] <| Maybe.Extra.values <| List.map (viewFigure (Array.toList model.data)) (Dict.toList model.graphs)
        ]


viewFigure : List Data -> ( String, Bool ) -> Maybe (Html.Html Msg)
viewFigure data ( figure, selected ) =
    if not selected then
        Nothing

    else
        Just
            (Html.figure [] [ drawFigure data <| stringToFigure figure ])


drawFigure : List Data -> Figure -> Html.Html Msg
drawFigure data figure =
    case figure of
        Plot ->
            Plot.draw { width = 300, height = 250 } <| toPlotData data



-- UTILS


toPlotData : List Data -> List Plot.Variable
toPlotData data = 
    List.map (\d -> {
        label = d.name,
        value = d.value,
        color = Just d.color
    }) data 

toggle : String -> Dict String Bool -> Dict String Bool
toggle figure dict =
    Dict.update figure (\maybeOld -> Maybe.map not maybeOld) dict


figureToString : Figure -> String
figureToString figure =
    case figure of
        Plot ->
            "plot"


stringToFigure : String -> Figure
stringToFigure figure =
    case figure of
        "Plot" ->
            Plot

        _ ->
            Plot
