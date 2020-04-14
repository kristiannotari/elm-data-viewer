module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import String



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


type Model
    = Normal State
    | CardCreation State Card


type alias State =
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
    = CardCreationStarted
    | CardCreationDone
    | CardCreationDiscarded
    | CardCreationChangedTitle String



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Normal { cards = defaultCards, labels = defaultLabels }, Cmd.none )


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


defaultNewCard : Card
defaultNewCard =
    { title = ""
    , spentTime = 0
    , plannedTime = Nothing
    , label = Nothing
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( CardCreationStarted, Normal state ) ->
            ( CardCreation state defaultNewCard, Cmd.none )

        ( CardCreationDone, CardCreation state card ) ->
            ( Normal { state | cards = List.append state.cards [ card ] }, Cmd.none )

        ( CardCreationDiscarded, CardCreation state _ ) ->
            ( Normal state, Cmd.none )

        ( CardCreationChangedTitle title, CardCreation state card ) ->
            ( CardCreation state { card | title = title }, Cmd.none )

        _ ->
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
        , viewCards model
        , viewCardCreation model
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
    [ Html.img [ Html.Events.onClick CardCreationStarted, Html.Attributes.src "/assets/add-black-36dp.svg" ] []
    , Html.img [ Html.Attributes.src "/assets/settings-black-36dp.svg" ] []
    ]


viewCards : Model -> Html Msg
viewCards model =
    case model of
        Normal state ->
            Html.section [] <| List.map viewCard state.cards

        CardCreation state _ ->
            Html.section [] <| List.map viewCard state.cards


viewCard : Card -> Html Msg
viewCard card =
    Html.div [] [ Html.text card.title ]


viewCardCreation : Model -> Html Msg
viewCardCreation model =
    case model of
        Normal _ ->
            Html.text ""

        CardCreation _ card ->
            Html.form [ Html.Events.onSubmit CardCreationDone ]
                [ Html.input [ Html.Events.onInput (\value -> CardCreationChangedTitle value), Html.Attributes.placeholder "Card Title", Html.Attributes.autofocus True ] [ Html.text card.title ]
                , Html.button [ Html.Attributes.disabled (String.isEmpty card.title), Html.Events.onClick CardCreationDone ] [ Html.text "Add" ]
                , Html.button [ Html.Events.onClick CardCreationDiscarded ] [ Html.text "Cancel" ]
                ]
