module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Color
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Maybe.Extra
import Model.Card as Card exposing (Card)
import Model.Label as Label exposing (Label)
import Regex
import String
import Task
import Time



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
    { state : State
    , scene : Scene
    }


type Scene
    = Normal
    | CardCreation Card
    | CardOver Card.Id
    | CardEditing ( Card.Id, Card )
    | CardDeletion Card.Id


type alias State =
    { cards : Card.Collection
    , labels : Label.Collection
    , active : Maybe Card.Id
    , sleep : Bool
    , restTime : Int
    }


type Msg
    = CardCreationStarted
    | CardCreationDone
    | CardAdded Card.Collection
    | CardCreationDiscarded
    | CardCreationChangedTitle String
    | CardCreationChangedLabel String
    | CardCreationChangedPlannedTime String
    | CardMouseEnter Card.Id
    | CardMouseLeave
    | CardEditingStarted
    | CardEditingChangedTitle String
    | CardEditingChangedLabel String
    | CardEditingChangedPlannedTime String
    | CardEditingDiscarded
    | CardEditingDone
    | CardDeletionStarted
    | CardDeletionCanceled
    | CardDeletionDone
    | CardActivated
    | SleepChanged
    | Tick Time.Posix
    | RestTick Time.Posix



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state =
            { cards = defaultCards
            , labels = defaultLabels
            , active = Nothing
            , sleep = False
            , restTime = 0
            }
      , scene = Normal
      }
    , Cmd.none
    )


defaultCards : Card.Collection
defaultCards =
    Card.emptyCollection


defaultLabels : Label.Collection
defaultLabels =
    Label.fromList
        [ ( "work", Label.new "Work" <| Color.fromRGB ( 56, 132, 231 ) )
        , ( "study", Label.new "Study" <| Color.fromRGB ( 255, 165, 0 ) )
        , ( "hobby", Label.new "Hobby" <| Color.fromRGB ( 115, 221, 101 ) )
        , ( "other", Label.new "Other" <| Color.fromRGB ( 80, 84, 88 ) )
        ]


defaultNewCard : Card
defaultNewCard =
    Card.new
        { title = ""
        , spentTime = 0
        , plannedTime = Nothing
        , label = Nothing
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        state =
            model.state
    in
    case ( msg, model.scene ) of
        ( CardCreationStarted, Normal ) ->
            ( { model | scene = CardCreation defaultNewCard }, Cmd.none )

        ( CardAdded cards, CardCreation _ ) ->
            ( { model | scene = Normal, state = { state | cards = cards } }, Cmd.none )

        ( CardCreationDone, CardCreation card ) ->
            ( model, Task.perform CardAdded (Card.insertCollection card state.cards) )

        ( CardCreationDiscarded, CardCreation _ ) ->
            ( { model | scene = Normal }, Cmd.none )

        ( CardCreationChangedTitle title, CardCreation card ) ->
            let
                newCard =
                    Card.getData card
            in
            ( { model | scene = CardCreation (Card.new { newCard | title = title }) }, Cmd.none )

        ( CardCreationChangedLabel labelValue, CardCreation card ) ->
            let
                newCard =
                    Card.getData card
            in
            ( { model | scene = CardCreation (Card.new { newCard | label = Just (Label.stringToId labelValue) }) }, Cmd.none )

        ( CardCreationChangedPlannedTime plannedTimeValue, CardCreation card ) ->
            let
                newCard =
                    Card.getData card
            in
            ( { model | scene = CardCreation (Card.new { newCard | plannedTime = plannedTime plannedTimeValue }) }, Cmd.none )

        ( CardMouseEnter cardId, Normal ) ->
            ( { model | scene = CardOver cardId }, Cmd.none )

        ( CardMouseLeave, CardOver _ ) ->
            ( { model | scene = Normal }, Cmd.none )

        ( CardActivated, CardOver cardId ) ->
            case Card.getCollection cardId state.cards of
                Just _ ->
                    ( { model | state = { state | active = Just cardId } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( CardEditingStarted, CardOver cardId ) ->
            case Card.getCollection cardId state.cards of
                Just card ->
                    ( { model | scene = CardEditing ( cardId, card ) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( CardEditingChangedTitle title, CardEditing ( cardId, card ) ) ->
            let
                newCard =
                    Card.getData card
            in
            ( { model | scene = CardEditing ( cardId, Card.new { newCard | title = title } ) }, Cmd.none )

        ( CardEditingChangedLabel labelValue, CardEditing ( cardId, card ) ) ->
            let
                newCard =
                    Card.getData card
            in
            ( { model | scene = CardEditing ( cardId, Card.new { newCard | label = Just (Label.stringToId labelValue) } ) }, Cmd.none )

        ( CardEditingChangedPlannedTime plannedTimeValue, CardEditing ( cardId, card ) ) ->
            let
                newCard =
                    Card.getData card
            in
            ( { model | scene = CardEditing ( cardId, Card.new { newCard | plannedTime = plannedTime plannedTimeValue } ) }, Cmd.none )

        ( CardEditingDiscarded, CardEditing _ ) ->
            ( { model | scene = Normal }, Cmd.none )

        ( CardEditingDone, CardEditing ( cardId, card ) ) ->
            ( { model | scene = Normal, state = { state | cards = Card.updateCollection cardId card state.cards } }, Cmd.none )

        ( CardDeletionStarted, CardOver cardId ) ->
            ( { model | scene = CardDeletion cardId }, Cmd.none )

        ( CardDeletionCanceled, CardDeletion _ ) ->
            ( { model | scene = Normal }, Cmd.none )

        ( CardDeletionDone, CardDeletion cardId ) ->
            ( { model | scene = Normal, state = { state | cards = Card.removeCollection cardId state.cards } }, Cmd.none )

        ( SleepChanged, _ ) ->
            ( { model | state = { state | sleep = not state.sleep } }, Cmd.none )

        ( Tick _, _ ) ->
            case state.active of
                Just cardId ->
                    case Card.getCollection cardId state.cards of
                        Just card ->
                            let
                                cardData =
                                    Card.getData card
                            in
                            ( { model | state = { state | cards = Card.updateCollection cardId (Card.new { cardData | spentTime = cardData.spentTime + 1 }) state.cards } }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( RestTick _, _ ) ->
            ( { model | state = { state | restTime = state.restTime + 1 } }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.state.sleep then
        Time.every 1000 RestTick

    else
        case model.state.active of
            Just _ ->
                Time.every 1000 Tick

            Nothing ->
                Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Day Focus"
    , body =
        [ Html.div [ Html.Attributes.id "container" ]
            [ viewHeaderBar model
            , viewStats model
            , viewCards model
            , viewCardCreation model
            , viewCardDeletion model
            ]
        ]
    }


viewHeaderBar : Model -> Html Msg
viewHeaderBar model =
    Html.header []
        [ Html.h1 [] [ Html.text "Elm Day Focus" ]
        , Html.div [] <| viewMenuItems model
        ]


viewStats : Model -> Html Msg
viewStats model =
    Html.section [ Html.Attributes.id "stats" ]
        [ Html.p [] [ Html.text <| "spent time: " ++ (formatSeconds <| totalSpentTime model) ++ " / " ++ (formatSeconds <| totalPlannedTime model) ]
        , Html.p [] [ Html.text <| "rest time: " ++ (formatSeconds <| totalRestTime model) ]
        ]


viewMenuItems : Model -> List (Html Msg)
viewMenuItems model =
    [ Html.img [ Html.Events.onClick CardCreationStarted, Html.Attributes.src "assets/add-black-36dp.svg" ] []
    , Html.img
        [ Html.Attributes.class <|
            if model.state.sleep then
                "sleeping"

            else
                ""
        , Html.Events.onClick SleepChanged
        , Html.Attributes.src "assets/brightness_3-black-36dp.svg"
        ]
        []
    , Html.img [ Html.Attributes.src "assets/settings-black-36dp.svg" ] []
    ]


viewCards : Model -> Html Msg
viewCards model =
    Html.Keyed.node "section" [ Html.Attributes.id "cards" ] <| Card.idMapCollection (viewCard model) model.state.cards


viewCard : Model -> Card.Id -> Card -> ( String, Html Msg )
viewCard model id card =
    let
        over =
            case model.scene of
                CardOver cardId ->
                    cardId == id

                _ ->
                    False

        details =
            [ Html.img [ Html.Events.onClick <| CardEditingStarted, Html.Attributes.class "over-icon", Html.Attributes.src "assets/create-black-36dp.svg" ] []
            , Html.img [ Html.Events.onClick <| CardDeletionStarted, Html.Attributes.class "over-icon", Html.Attributes.src "assets/delete-black-36dp.svg" ] []
            ]

        cardData =
            Card.getData card

        active =
            Maybe.Extra.unwrap False (\cardId -> cardId == id) model.state.active
    in
    ( Card.idToString id
    , Html.div
        [ Html.Attributes.class <|
            if active then
                "card active"

            else
                "card"
        , Html.Attributes.class <|
            if over then
                "over"

            else
                ""
        , Html.Events.onMouseEnter <| CardMouseEnter id
        , Html.Events.onMouseLeave CardMouseLeave
        , Html.Events.onClick CardActivated
        ]
      <|
        List.append ([ viewCardTitle cardData.title, viewLabel model cardData.label ] ++ viewCardTime cardData) details
    )


viewCardTitle : String -> Html Msg
viewCardTitle title =
    Html.p [ Html.Attributes.class "title" ] [ Html.text title ]


viewLabel : Model -> Maybe Label.Id -> Html Msg
viewLabel model maybeLabel =
    case maybeLabel of
        Just labelId ->
            case Label.getCollection labelId model.state.labels of
                Just label ->
                    let
                        labelData =
                            Label.getData label
                    in
                    Html.p (Html.Attributes.class "label" :: labelColors labelData) [ Html.text labelData.name ]

                Nothing ->
                    Html.text ""

        Nothing ->
            Html.text ""


labelColors : Label.Data -> List (Html.Attribute Msg)
labelColors labelData =
    let
        color =
            if Color.luminance labelData.color >= 0.5 then
                "#24292e"

            else
                "#ffffff"
    in
    [ Html.Attributes.style "color" color, Html.Attributes.style "background-color" <| Color.toRGBString labelData.color ]


viewCardTime : Card.Data -> List (Html Msg)
viewCardTime cardData =
    let
        plannedTimeValue =
            Maybe.withDefault 0 cardData.plannedTime

        plannedTimeFormatted =
            if plannedTimeValue /= 0 then
                formatSeconds plannedTimeValue

            else
                "-"
    in
    [ Html.p [ Html.Attributes.class "spent-time" ] [ Html.text <| formatSeconds cardData.spentTime ]
    , Html.p [] [ Html.text "/" ]
    , Html.p [ Html.Attributes.class "planned-time" ] [ Html.text plannedTimeFormatted ]
    ]


viewCardCreation : Model -> Html Msg
viewCardCreation model =
    case model.scene of
        CardCreation card ->
            let
                cardData =
                    Card.getData card

                plannedTimeValueAttribute =
                    case cardData.plannedTime of
                        Just _ ->
                            []

                        Nothing ->
                            [ Html.Attributes.class "invalid" ]
            in
            Html.div [ Html.Attributes.id "obscurer" ]
                [ Html.div [ Html.Attributes.id "card-editor" ]
                    [ Html.input [ Html.Attributes.class "title", Html.Events.onInput (\value -> CardCreationChangedTitle value), Html.Attributes.placeholder "Card Title", Html.Attributes.autofocus True, Html.Attributes.value cardData.title ] []
                    , Html.div [ Html.Attributes.class "details" ]
                        [ Html.div [ Html.Attributes.class "labeled-input" ]
                            [ Html.label [] [ Html.text "label" ]
                            , Html.select [ Html.Events.onInput (\value -> CardCreationChangedLabel value) ] <| Html.option [ Html.Attributes.value "" ] [ Html.text "-" ] :: Label.idMapCollection (viewLabelOption Nothing) model.state.labels
                            ]
                        , Html.div [ Html.Attributes.class "labeled-input" ]
                            [ Html.label [] [ Html.text "planned time" ]
                            , Html.input ([ Html.Attributes.class "planned-time", Html.Events.onInput (\value -> CardCreationChangedPlannedTime value) ] ++ plannedTimeValueAttribute) []
                            ]
                        ]
                    , Html.div [ Html.Attributes.class "buttons" ]
                        [ Html.button [ Html.Attributes.disabled (String.isEmpty cardData.title), Html.Events.onClick CardCreationDone ] [ Html.text "Add" ]
                        , Html.button [ Html.Events.onClick CardCreationDiscarded ] [ Html.text "Cancel" ]
                        ]
                    ]
                ]

        CardEditing ( _, card ) ->
            let
                cardData =
                    Card.getData card

                plannedTimeValueAttribute =
                    case cardData.plannedTime of
                        Just pt ->
                            [ Html.Attributes.placeholder <| formatSeconds pt ]

                        Nothing ->
                            [ Html.Attributes.class "invalid" ]
            in
            Html.div [ Html.Attributes.id "obscurer" ]
                [ Html.div [ Html.Attributes.id "card-editor" ]
                    [ Html.input [ Html.Attributes.class "title", Html.Events.onInput (\value -> CardEditingChangedTitle value), Html.Attributes.placeholder "Card Title", Html.Attributes.autofocus True, Html.Attributes.value cardData.title ] []
                    , Html.div [ Html.Attributes.class "details" ]
                        [ Html.div [ Html.Attributes.class "labeled-input" ]
                            [ Html.label [] [ Html.text "label" ]
                            , Html.select [ Html.Events.onInput (\value -> CardEditingChangedLabel value) ] <| Html.option [ Html.Attributes.value "" ] [ Html.text "label" ] :: Label.idMapCollection (viewLabelOption cardData.label) model.state.labels
                            ]
                        , Html.div [ Html.Attributes.class "labeled-input" ]
                            [ Html.label [] [ Html.text "planned time" ]
                            , Html.input ([ Html.Attributes.class "planned-time", Html.Events.onInput (\value -> CardEditingChangedPlannedTime value) ] ++ plannedTimeValueAttribute) []
                            ]
                        ]
                    , Html.div [ Html.Attributes.class "buttons" ]
                        [ Html.button [ Html.Attributes.disabled (String.isEmpty cardData.title), Html.Events.onClick CardEditingDone ] [ Html.text "Change" ]
                        , Html.button [ Html.Events.onClick CardEditingDiscarded ] [ Html.text "Cancel" ]
                        ]
                    ]
                ]

        _ ->
            Html.text ""


viewCardDeletion : Model -> Html Msg
viewCardDeletion model =
    case model.scene of
        CardDeletion cardId ->
            case Card.getCollection cardId model.state.cards of
                Just card ->
                    let
                        cardData =
                            Card.getData card
                    in
                    Html.div [ Html.Attributes.id "obscurer" ]
                        [ Html.div [ Html.Attributes.id "card-deletor" ]
                            [ Html.p [] [ Html.text <| "Are you sure you want to delete this card:" ]
                            , Html.p [ Html.Attributes.class "title" ] [ Html.text cardData.title ]
                            , Html.div [ Html.Attributes.class "buttons" ]
                                [ Html.button [ Html.Events.onClick CardDeletionDone ] [ Html.text "Yes" ]
                                , Html.button [ Html.Events.onClick CardDeletionCanceled ] [ Html.text "No" ]
                                ]
                            ]
                        ]

                Nothing ->
                    Html.text ""

        _ ->
            Html.text ""


viewLabelOption : Maybe Label.Id -> Label.Id -> Label -> Html Msg
viewLabelOption cardLabelId labelId label =
    let
        labelData =
            Label.getData label

        selected =
            case cardLabelId of
                Just id ->
                    Label.idToString id == Label.idToString labelId

                Nothing ->
                    False
    in
    Html.option [ Html.Attributes.value <| Label.idToString labelId, Html.Attributes.selected selected ] [ Html.text labelData.name ]



-- UTILS


formatSeconds : Int -> String
formatSeconds spentTime =
    let
        hours =
            spentTime // 60 // 60

        minutes =
            modBy 60 <| spentTime // 60

        seconds =
            modBy 60 spentTime
    in
    (if hours > 0 then
        String.fromInt hours ++ "h "

     else
        ""
    )
        ++ (if minutes > 0 then
                String.fromInt minutes ++ "m "

            else
                ""
           )
        ++ String.fromInt seconds
        ++ "s"


totalSpentTime : Model -> Int
totalSpentTime model =
    List.foldl (\time acc -> acc + time) 0 <|
        Card.idMapCollection
            (\_ card ->
                let
                    cardData =
                        Card.getData card
                in
                cardData.spentTime
            )
            model.state.cards


totalRestTime : Model -> Int
totalRestTime model =
    model.state.restTime


plannedTime : String -> Maybe Int
plannedTime value =
    let
        regex =
            Maybe.withDefault Regex.never <| Regex.fromString "(([1-9]+[0-9]*)h)?\\s*(([1-9]+[0-9]*)m)?\\s*(([1-9]+[0-9]*)s)?"
    in
    case Regex.find regex value of
        [ match ] ->
            case match.submatches of
                [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ] ->
                    Nothing

                _ ->
                    let
                        hMatches =
                            List.take 2 match.submatches

                        mMatches =
                            List.take 2 <| List.drop 2 match.submatches

                        sMatches =
                            List.take 2 <| List.drop 4 match.submatches

                        h =
                            case hMatches of
                                [ Just _, Just hValue ] ->
                                    Maybe.withDefault 0 <| String.toInt hValue

                                _ ->
                                    0

                        m =
                            case mMatches of
                                [ Just _, Just mValue ] ->
                                    Maybe.withDefault 0 <| String.toInt mValue

                                _ ->
                                    0

                        s =
                            case sMatches of
                                [ Just _, Just sValue ] ->
                                    Maybe.withDefault 0 <| String.toInt sValue

                                _ ->
                                    0
                    in
                    Just <| h * 60 * 60 + m * 60 + s

        _ ->
            Nothing


totalPlannedTime : Model -> Int
totalPlannedTime model =
    List.foldl (\pt acc -> acc + pt) 0 <|
        Card.idMapCollection
            (\_ card ->
                let
                    cardData =
                        Card.getData card
                in
                Maybe.withDefault 0 cardData.plannedTime
            )
            model.state.cards
