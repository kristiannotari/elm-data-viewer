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
    | CardOver State Int
    | CardEditing State Card Int


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
    | CardMouseEnter Int
    | CardMouseLeave
    | CardEditingStarted Card
    | CardEditingChangedTitle String
    | CardEditingDiscarded
    | CardEditingDone



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

        ( CardMouseEnter index, Normal state ) ->
            ( CardOver state index, Cmd.none )
            
        ( CardMouseLeave, CardOver state _ ) ->
            ( Normal state, Cmd.none )
        
        ( CardEditingStarted card, CardOver state index ) ->
            ( CardEditing state card index, Cmd.none )

        ( CardEditingChangedTitle title, CardEditing state card index ) ->
            ( CardEditing state { card | title = title } index, Cmd.none )

        ( CardEditingDiscarded, CardEditing state _ _ ) ->
            ( Normal state, Cmd.none )

        ( CardEditingDone, CardEditing state card index ) ->
            ( Normal { state | cards = List.indexedMap (\i c -> if i == index then card else c ) state.cards}, Cmd.none )

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
    let
        state = case model of
            Normal s -> s
            CardCreation s _ -> s
            CardOver s _ -> s
            CardEditing s _ _ -> s
    in
        Html.section [] <| List.indexedMap (viewCard model) state.cards


viewCard : Model -> Int -> Card -> Html Msg
viewCard model index card =
    let
        details = case model of
            CardOver state overIndex ->
                if overIndex == index then
                    Html.img [ Html.Events.onClick <| CardEditingStarted card, Html.Attributes.class "edit-icon", Html.Attributes.src "/assets/create-black-36dp.svg" ] []
                else
                    Html.text ""
            _ ->
                Html.text ""
    in 
        Html.div [Html.Events.onMouseEnter <| CardMouseEnter index, Html.Events.onMouseLeave <| CardMouseLeave ] <| List.append [Html.text card.title] [details]

viewCardCreation : Model -> Html Msg
viewCardCreation model =
    case model of
        CardCreation _ card ->
            Html.form [ Html.Events.onSubmit CardCreationDone ]
                [ Html.input [ Html.Events.onInput (\value -> CardCreationChangedTitle value), Html.Attributes.placeholder "Card Title", Html.Attributes.autofocus True, Html.Attributes.value card.title ] []
                , Html.button [ Html.Attributes.disabled (String.isEmpty card.title), Html.Events.onClick CardCreationDone ] [ Html.text "Add" ]
                , Html.button [ Html.Events.onClick CardCreationDiscarded ] [ Html.text "Cancel" ]
                ]
        CardEditing _ card _ ->
            Html.form [ Html.Attributes.class card.title, Html.Events.onSubmit CardEditingDone ]
                [ Html.input [ Html.Events.onInput (\value -> CardEditingChangedTitle value), Html.Attributes.placeholder "Card Title", Html.Attributes.autofocus True, Html.Attributes.value card.title] []
                , Html.button [ Html.Attributes.disabled (String.isEmpty card.title), Html.Events.onClick CardEditingDone ] [ Html.text "Change" ]
                , Html.button [ Html.Events.onClick CardEditingDiscarded ] [ Html.text "Cancel" ]
                ]
        _ ->
            Html.text ""

