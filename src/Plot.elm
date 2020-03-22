module Plot exposing (name, draw)

import Html
import Svg
import Svg.Attributes
import Data exposing (Data)

name : String
name = "Plot"

draw : List Data -> Html.Html msg
draw data =
    Svg.svg
        [ Svg.Attributes.width "300"
        , Svg.Attributes.height "250"
        , Svg.Attributes.viewBox "0 0 300 250"
        ]
        (List.append
            [ Svg.line
                [ Svg.Attributes.x1 "5"
                , Svg.Attributes.y1 "245"
                , Svg.Attributes.x2 "300"
                , Svg.Attributes.y2 "245"
                , Svg.Attributes.stroke "black"
                ]
                []
            , Svg.line
                [ Svg.Attributes.x1 "5"
                , Svg.Attributes.y1 "245"
                , Svg.Attributes.x2 "5"
                , Svg.Attributes.y2 "0"
                , Svg.Attributes.stroke "black"
                ]
                []
            ]
            (List.indexedMap
                drawPlotPoint
                data
            )
        )


drawPlotPoint : Int -> Data -> Svg.Svg msg
drawPlotPoint index data =
    case modBy 2 index of
        0 ->
            Svg.line
                [ Svg.Attributes.x1 <| String.fromFloat <| data.value + 5
                , Svg.Attributes.y1 "250"
                , Svg.Attributes.x2 <| String.fromFloat <| data.value + 5
                , Svg.Attributes.y2 "240"
                , Svg.Attributes.stroke data.color
                ]
                []

        _ ->
            Svg.line
                [ Svg.Attributes.x1 "0"
                , Svg.Attributes.y1 <| String.fromFloat <| 245 - data.value
                , Svg.Attributes.x2 "10"
                , Svg.Attributes.y2 <| String.fromFloat <| 245 - data.value
                , Svg.Attributes.stroke data.color
                ]
                []