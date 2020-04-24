module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import String
import Model.Card as Card exposing (Card)
import Model.Label as Label exposing (Label)
import Task


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
    {
        state: State,
        scene: Scene
    }

type Scene
    = Normal
    | CardCreation Card
    | CardOver Card.Id
    | CardEditing (Card.Id, Card)


type alias State =
    { cards : Card.Collection
    , labels : Label.Collection
    , active: Maybe Card.Id
    , sleep: Bool
    }


type Msg
    = CardCreationStarted
    | CardCreationDone
    | CardAdded Card.Collection
    | CardCreationDiscarded
    | CardCreationChangedTitle String
    | CardCreationChangedLabel String
    | CardMouseEnter Card.Id
    | CardMouseLeave
    | CardEditingStarted
    | CardEditingChangedTitle String
    | CardEditingChangedLabel String
    | CardEditingDiscarded
    | CardEditingDone



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { 
        state = { 
            cards = defaultCards,
            labels = defaultLabels,
            active = Nothing,
            sleep = False
            },
        scene = Normal
        }
        , Cmd.none )


defaultCards : Card.Collection
defaultCards = Card.emptyCollection


defaultLabels : Label.Collection
defaultLabels =
    Label.fromList [
        ("red", Label.new "Red" "red"),
        ("orange", Label.new "Orange" "orange"),
        ("yellow", Label.new "Yellow" "yellow")
    ]


defaultNewCard : Card
defaultNewCard =
    Card.new { title = ""
                , spentTime = 0
                , plannedTime = Nothing
                , label = Nothing
                }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        state = model.state
    in
        case ( msg, model.scene ) of
            ( CardCreationStarted, Normal ) ->
                ( { model | scene = CardCreation defaultNewCard }, Cmd.none )

            ( CardAdded cards, CardCreation card ) ->
                ( { model | scene = Normal, state = { state | cards = cards }}, Cmd.none )

            ( CardCreationDone, CardCreation card ) ->
                ( model, Task.perform CardAdded (Card.insertCollection card state.cards) )

            ( CardCreationDiscarded, CardCreation _ ) ->
                ( { model | scene = Normal }, Cmd.none )

            ( CardCreationChangedTitle title, CardCreation card ) ->
                let newCard = Card.getData card in
                ( { model | scene = CardCreation (Card.new { newCard | title = title }) }, Cmd.none )

            ( CardCreationChangedLabel labelValue, CardCreation card ) ->
                let newCard = Card.getData card in
                ( { model | scene = CardCreation (Card.new { newCard | label = Just (Label.stringToId labelValue) }) }, Cmd.none )

            ( CardMouseEnter cardId, Normal ) ->
                ( { model | scene = CardOver cardId }, Cmd.none )
                
            ( CardMouseLeave, CardOver _ ) ->
                ( { model | scene = Normal }, Cmd.none )
            
            ( CardEditingStarted, CardOver cardId ) ->
                case Card.getCollection cardId state.cards of
                    Just card -> 
                        ( { model | scene = CardEditing (cardId, card)}, Cmd.none )
                    Nothing -> 
                        ( model, Cmd.none )

            ( CardEditingChangedTitle title, CardEditing (cardId, card) ) ->
                let newCard = Card.getData card in
                ( { model | scene = CardEditing (cardId, Card.new { newCard | title = title }) }, Cmd.none )

            ( CardEditingChangedLabel labelValue, CardEditing (cardId, card) ) ->
                let newCard = Card.getData card in
                ( { model | scene = CardEditing (cardId, Card.new { newCard | label = Just (Label.stringToId labelValue) }) }, Cmd.none )

            ( CardEditingDiscarded, CardEditing _ ) ->
                ( { model | scene = Normal }, Cmd.none )

            ( CardEditingDone, CardEditing (cardId, card) ) ->
                ( { model | scene = Normal, state = { state | cards = Card.updateCollection cardId card state.cards }}, Cmd.none )

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
    Html.Keyed.node "section" [] <| Card.idMapCollection (viewCard model) model.state.cards


viewCard : Model -> Card.Id -> Card -> (String, Html Msg)
viewCard model id card =
    let
        details = case model.scene of
            CardOver cardId ->
                if cardId == id then
                    Html.img [ Html.Events.onClick <| CardEditingStarted, Html.Attributes.class "edit-icon", Html.Attributes.src "/assets/create-black-36dp.svg" ] []
                else
                    Html.text ""
            _ ->
                Html.text ""
        cardData = Card.getData card
        labelDetails = case cardData.label of
            Just labelId ->
                case Label.getCollection labelId model.state.labels of
                    Just label -> 
                        let labelData = Label.getData label in Html.p [ Html.Attributes.style "background-color" labelData.color ] [ Html.text labelData.name ]
                    Nothing ->
                        Html.text ""
            Nothing ->
                Html.text ""
    in 
        (Card.idToString id, Html.div [Html.Events.onMouseEnter <| CardMouseEnter id, Html.Events.onMouseLeave <| CardMouseLeave ] <| List.append [Html.text cardData.title, labelDetails] [details])

viewCardCreation : Model -> Html Msg
viewCardCreation model =
    case model.scene of
        CardCreation card ->
            let cardData = Card.getData card in
            Html.form [ Html.Events.onSubmit CardCreationDone ]
                [ Html.input [ Html.Events.onInput (\value -> CardCreationChangedTitle value), Html.Attributes.placeholder "Card Title", Html.Attributes.autofocus True, Html.Attributes.value cardData.title ] []
                , Html.select [ Html.Events.onInput (\value -> CardCreationChangedLabel value)] <| (Html.option [ Html.Attributes.value "" ] [ Html.text "-" ]) :: Label.idMapCollection (viewLabelOption Nothing) model.state.labels
                , Html.button [ Html.Attributes.disabled (String.isEmpty cardData.title), Html.Events.onClick CardCreationDone ] [ Html.text "Add" ]
                , Html.button [ Html.Events.onClick CardCreationDiscarded ] [ Html.text "Cancel" ]
                ]
        CardEditing (cardId, card) ->
            let cardData = Card.getData card in
            Html.form [ Html.Events.onSubmit CardEditingDone ]
                [ Html.input [ Html.Events.onInput (\value -> CardEditingChangedTitle value), Html.Attributes.placeholder "Card Title", Html.Attributes.autofocus True, Html.Attributes.value cardData.title] []
                , Html.select [ Html.Events.onInput (\value -> CardEditingChangedLabel value)] <| (Html.option [ Html.Attributes.value "" ] [ Html.text "label" ]) :: Label.idMapCollection (viewLabelOption cardData.label) model.state.labels
                , Html.button [ Html.Attributes.disabled (String.isEmpty cardData.title), Html.Events.onClick CardEditingDone ] [ Html.text "Change" ]
                , Html.button [ Html.Events.onClick CardEditingDiscarded ] [ Html.text "Cancel" ]
                ]
        _ ->
            Html.text ""


viewLabelOption: Maybe Label.Id -> Label.Id -> Label -> Html Msg
viewLabelOption cardLabelId labelId label =
    let
        labelData = Label.getData label
        selected = case cardLabelId of
            Just id -> Label.idToString id == Label.idToString labelId
            Nothing -> False
    in Html.option [ Html.Attributes.value <| Label.idToString labelId, Html.Attributes.selected selected ] [ Html.text labelData.name ]