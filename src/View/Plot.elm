module View.Plot exposing (Size, Variable, name, draw)

import Html
import Svg
import Svg.Attributes

type alias Size =
    {
        width : Float,
        height : Float
    }

type alias Variable = 
 {
     label : String,
     value : Float,
     color : Maybe String
 }

defaults = 
    {
        color = "black"
    }

name : String
name = "Plot"

draw : Size -> List Variable -> Html.Html msg
draw size data =
    let
        width = String.fromFloat size.width
        height = String.fromFloat size.height
    in
    
    Svg.svg
        [ Svg.Attributes.width width
        , Svg.Attributes.height height
        , Svg.Attributes.viewBox ("0 0" ++ " " ++ width ++ " " ++ height)
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


drawPlotPoint : Int -> Variable -> Svg.Svg msg
drawPlotPoint index variable =
    case modBy 2 index of
        0 ->
            Svg.line
                [ Svg.Attributes.x1 <| String.fromFloat <| variable.value + 5
                , Svg.Attributes.y1 "250"
                , Svg.Attributes.x2 <| String.fromFloat <| variable.value + 5
                , Svg.Attributes.y2 "240"
                , Svg.Attributes.stroke <| Maybe.withDefault defaults.color variable.color
                ]
                []

        _ ->
            Svg.line
                [ Svg.Attributes.x1 "0"
                , Svg.Attributes.y1 <| String.fromFloat <| 245 - variable.value
                , Svg.Attributes.x2 "10"
                , Svg.Attributes.y2 <| String.fromFloat <| 245 - variable.value
                , Svg.Attributes.stroke <| Maybe.withDefault defaults.color variable.color
                ]
                []