module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { cards : List Card
    , labels : List Label
    }


type alias Card =
    { title : String
    , spentTime : Int
    , plannedTime : Maybe Int
    , label : Maybe Label
    }


type alias Label =
    { color : String
    , name : String
    }


type Msg
    = Undefined



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cards = defaultCards, labels = defaultLabels }, Cmd.none )


defaultCards : List Card
defaultCards =
    [ { title = "Card 1", spentTime = 0, plannedTime = Nothing, label = Nothing }
    , { title = "Card 2", spentTime = 0, plannedTime = Nothing, label = Nothing }
    , { title = "Card 3", spentTime = 0, plannedTime = Nothing, label = Nothing }
    ]


defaultLabels : List Label
defaultLabels =
    [ { name = "Orange"
      , color = "orange"
      }
    , { name = "Red"
      , color = "red"
      }
    , { name = "Yellow"
      , color = "yellow"
      }
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Day Focus"
    , body =
        [ viewHeaderBar
        , viewCards model.cards
        ]
    }


viewHeaderBar : Html Msg
viewHeaderBar =
    Html.header []
        (Html.h1 [] [ Html.text "Elm Day Focus" ]
            :: viewMenuItems
        )


viewMenuItems : List (Html Msg)
viewMenuItems =
    [ Html.img [ Html.Attributes.src "/assets/add-black-36dp.svg" ] []
    , Html.img [ Html.Attributes.src "/assets/settings-black-36dp.svg" ] []
    ]


viewCards : List Card -> Html Msg
viewCards cards =
    Html.section [] <| List.map viewCard cards


viewCard : Card -> Html Msg
viewCard card =
    Html.div [] [ Html.text card.title ]
