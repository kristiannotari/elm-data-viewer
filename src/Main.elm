module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html
import Html.Attributes
import String
import Svg
import Svg.Attributes
import Maybe.Extra



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
    { data : List Data
    , graphs : List Graph
    }


type alias Data =
    { name : String
    , value : Float
    , color : String
    }


type alias Graph =
    { figure : Figure
    , selected : Bool
    }

type Figure =
    Plot


type Msg
    = Msg1
    | Msg2



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = [ { name = "x", value = 3.14, color = "#ff2312" } ], graphs = [ { figure = Plot, selected = True } ] }, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        Msg2 ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Data Viewer"
    , body =
        [ viewHeader model
        , viewMain model
        ]
    }


viewHeader : Model -> Html.Html Msg
viewHeader model =
    Html.header []
        [ Html.ul []
            [ Html.li [] [ Html.text "Test" ]
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
        , Html.div [] (List.map viewData model.data)
        ]


viewData : Data -> Html.Html Msg
viewData data =
    Html.div []
        [ Html.input [ Html.Attributes.type_ "text", Html.Attributes.value data.name ] []
        , Html.input [ Html.Attributes.type_ "number", Html.Attributes.value (String.fromFloat data.value) ] []
        , Html.input [ Html.Attributes.type_ "text", Html.Attributes.value data.color ] []
        ]


viewGraphTypesList : Model -> Html.Html Msg
viewGraphTypesList model =
    Html.section []
        [ Html.header [] [ Html.text "Graphs" ]
        , Html.p [] [ Html.text "Select one or more graphs" ]
        , Html.div [] (List.map viewGraphType model.graphs)
        ]


viewGraphType : Graph -> Html.Html Msg
viewGraphType graph =
    Html.div []
        [ Html.input [ Html.Attributes.type_ "checkbox" ] []
        , Html.label [ Html.Attributes.for "" ] [ Html.text (figureToString graph.figure) ]
        ]


viewFiguresList : Model -> Html.Html Msg
viewFiguresList model =
    Html.section []
        [ Html.header [] [ Html.text "Figures" ]
        , Html.div [] (List.map (viewFigure model.data) model.graphs |> Maybe.Extra.values)
        ]

viewFigure : List Data -> Graph -> Maybe (Html.Html Msg)
viewFigure data graph =
    if not graph.selected then Nothing
    else
        Just (
            Html.figure [] [drawFigure data graph]
        )


drawFigure : List Data -> Graph -> Html.Html Msg
drawFigure data graph = 
    case graph.figure of
        Plot -> drawPlot data


drawPlot : List Data -> Html.Html Msg
drawPlot data =
    Svg.svg [
        Svg.Attributes.width "300",
        Svg.Attributes.height "250",
        Svg.Attributes.viewBox "0 0 300 250"
    ] [
        Svg.rect [ Svg.Attributes.x "10"
        , Svg.Attributes.y "10"
        , Svg.Attributes.width "100"
        , Svg.Attributes.height "100"
        , Svg.Attributes.rx "15"
        , Svg.Attributes.ry "15"
        ] []
    ]
    


-- UTIL

figureToString : Figure -> String
figureToString figure =
    case figure of
        Plot -> "plot"
        
