module View.Plot exposing (Size, Variable, draw, name)

import Html
import Svg
import Svg.Attributes


type alias Size =
    { width : Float
    , height : Float
    }


type alias Variable =
    { label : String
    , value : Float
    , scaledValue : Maybe Float
    , color : Maybe String
    }


defaults :
    { color : String
    , padding : Float
    }
defaults =
    { color = "black"
    , padding = 5
    }



-- VIEW


draw : Size -> List Variable -> Html.Html msg
draw size data =
    let
        widthString =
            String.fromFloat size.width

        heightString =
            String.fromFloat size.height

        scaledData =
            scale data size
    in
    Svg.svg
        [ Svg.Attributes.width widthString
        , Svg.Attributes.height heightString
        , Svg.Attributes.viewBox ("0 0" ++ " " ++ widthString ++ " " ++ heightString)
        ]
        (List.append
            [ Svg.line
                -- x
                [ Svg.Attributes.x1 <| String.fromFloat defaults.padding
                , Svg.Attributes.y1 <| String.fromFloat (size.height - defaults.padding)
                , Svg.Attributes.x2 widthString
                , Svg.Attributes.y2 <| String.fromFloat (size.height - defaults.padding)
                , Svg.Attributes.stroke defaults.color
                ]
                []
            , Svg.line
                -- y
                [ Svg.Attributes.x1 <| String.fromFloat defaults.padding
                , Svg.Attributes.y1 <| String.fromFloat (size.height - defaults.padding)
                , Svg.Attributes.x2 <| String.fromFloat defaults.padding
                , Svg.Attributes.y2 "0"
                , Svg.Attributes.stroke defaults.color
                ]
                []
            ]
            (List.indexedMap
                (drawPlotPoint
                    size
                )
                scaledData
            )
        )


drawPlotPoint : Size -> Int -> Variable -> Svg.Svg msg
drawPlotPoint size index variable =
    let
        value =
            Maybe.withDefault variable.value variable.scaledValue + Debug.log (Debug.toString variable) 0
    in
    case modBy 2 index of
        0 ->
            Svg.line
                [ Svg.Attributes.x1 <| String.fromFloat <| value + 5
                , Svg.Attributes.y1 <| String.fromFloat size.height
                , Svg.Attributes.x2 <| String.fromFloat <| value + 5
                , Svg.Attributes.y2 <| String.fromFloat (size.height - defaults.padding * 2)
                , Svg.Attributes.stroke <| Maybe.withDefault defaults.color variable.color
                ]
                []

        _ ->
            Svg.line
                [ Svg.Attributes.x1 "0"
                , Svg.Attributes.y1 <| String.fromFloat <| (size.height - defaults.padding) - value
                , Svg.Attributes.x2 <| String.fromFloat (defaults.padding * 2)
                , Svg.Attributes.y2 <| String.fromFloat <| (size.height - defaults.padding) - value
                , Svg.Attributes.stroke <| Maybe.withDefault defaults.color variable.color
                ]
                []



-- UTILS


name : String
name =
    "Plot"


scale : List Variable -> Size -> List Variable
scale data size =
    let
        scale_ x =
            let
                dimension =
                    if x then
                        size.width - defaults.padding

                    else
                        size.height - defaults.padding

                max_ =
                    Maybe.withDefault dimension <| List.maximum (List.map (\v -> v.value) data)
            in
            \v -> dimension * v / max dimension max_
    in
    List.indexedMap
        (\index variable ->
            { variable
                | scaledValue =
                    case variable.scaledValue of
                        Just scaledValue ->
                            Just scaledValue

                        Nothing ->
                            Just <| scale_ (modBy 2 index == 0) variable.value
            }
        )
        data
