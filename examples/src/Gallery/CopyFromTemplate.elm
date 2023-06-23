module Gallery.CopyFromTemplate exposing (Model, Msg, initialModel, main, subscriptions, update, view)

import Browser
import DnDList
import DnDList.Groups
import Html
import Html.Attributes
import List
import List.Extra



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- DATA


type CardType
    = Template
    | Item


type alias Card =
    { cardType : CardType
    , description : String
    }


type alias DragOrigin =
    { card : Card
    , index : Int
    }


data : List Card
data =
    [ Card Template "A"
    , Card Template "B"
    , Card Template "C"
    , Card Template "D"
    , Card Template "E"
    , Card Template ""
    , Card Item "C"
    , Card Item "D"
    , Card Item ""
    ]



-- SYSTEM


cardConfig : DnDList.Groups.Config Card
cardConfig =
    { beforeUpdate = \_ _ list -> list
    , listen = DnDList.Groups.OnDrag
    , operation = DnDList.Groups.Rotate
    , groups =
        { listen = DnDList.Groups.OnDrag
        , operation = DnDList.Groups.InsertBefore
        , comparator = comparator
        , setter = setter
        }
    }


comparator : Card -> Card -> Bool
comparator card1 card2 =
    card1.cardType == card2.cardType


setter : Card -> Card -> Card
setter card1 card2 =
    { card2 | cardType = card1.cardType }


cardSystem : DnDList.Groups.System Card Msg
cardSystem =
    DnDList.Groups.create cardConfig CardMoved



-- MODEL


type alias Model =
    { cardDnD : DnDList.Groups.Model
    , cards : List Card
    , maybeDragOrigin : Maybe DragOrigin
    }


initialModel : Model
initialModel =
    { cardDnD = cardSystem.model
    , cards = data
    , maybeDragOrigin = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ cardSystem.subscriptions model.cardDnD
        ]



-- UPDATE


type Msg
    = CardMoved DnDList.Groups.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        CardMoved msg ->
            let
                pre =
                    cardSystem.info model.cardDnD

                ( cardDnD, cards ) =
                    cardSystem.update msg model.cardDnD model.cards

                post =
                    cardSystem.info cardDnD

                templateCardCount =
                    cards
                        |> List.Extra.takeWhile (\card -> card.cardType == Template)
                        |> List.length

                ( maybeDragOrigin, updatedCards ) =
                    case ( pre, post ) of
                        ( Just preInfo, Just postInfo ) ->
                            -- Dragging
                            case model.maybeDragOrigin of
                                Nothing ->
                                    -- Drag start
                                    ( cards
                                        |> List.drop preInfo.dragIndex
                                        |> List.head
                                        |> Maybe.map (\card -> { card = card, index = preInfo.dragIndex })
                                    , cards
                                    )

                                Just _ ->
                                    ( model.maybeDragOrigin
                                    , cards
                                    )

                        ( Just preInfo, Nothing ) ->
                            -- Dropped
                            case model.maybeDragOrigin of
                                Nothing ->
                                    ( Nothing, cards )

                                Just dragOrigin ->
                                    if dragOrigin.card.cardType == Template && preInfo.dropIndex >= templateCardCount then
                                        ( Nothing
                                        , (cards |> List.take dragOrigin.index)
                                            ++ (dragOrigin.card :: (cards |> List.drop dragOrigin.index))
                                        )

                                    else
                                        ( Nothing, cards )

                        _ ->
                            -- Before drag
                            ( model.maybeDragOrigin, cards )
            in
            ( { model
                | cardDnD = cardDnD
                , cards = updatedCards
                , maybeDragOrigin = maybeDragOrigin
              }
            , cardSystem.commands cardDnD
            )



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        columns : List (List Card)
        columns =
            model.cards
                |> gatherByCardType

        calculateOffset : Int -> Int
        calculateOffset columnIndex =
            columns |> List.map List.length |> List.take columnIndex |> List.foldl (+) 0
    in
    Html.section []
        [ columns
            |> List.indexedMap (\i column -> columnView model (calculateOffset i) i column)
            |> Html.div boardStyles
        , cardGhostView model
        ]


columnView : Model -> Int -> Int -> List Card -> Html.Html Msg
columnView model offset index cards =
    let
        heading : Heading
        heading =
            getCardTypeHeading (List.take 1 cards)

        columnId : String
        columnId =
            "column-" ++ String.fromInt index
    in
    Html.div
        (Html.Attributes.id columnId
            :: columnStyles "transparent"
        )
        [ Html.h3
            (columnHeadingStyles heading.color)
            [ Html.text heading.title ]
        , case ( index, model.maybeDragOrigin ) of
            ( 0, Just dragOrigin ) ->
                -- Special handling for the first column when dragging
                let
                    droppingToItem =
                        case cardSystem.info model.cardDnD of
                            Just { dropIndex } ->
                                dropIndex >= List.length cards

                            _ ->
                                False

                    ( dropEvents, cards_ ) =
                        if dragOrigin.card.cardType == Template then
                            -- If dragging a Template, make the whole first column droppable to "reset"
                            -- the dragged Template back to its original position
                            ( cardSystem.dropEvents dragOrigin.index "reset"
                            , if droppingToItem then
                                -- When dragging a Template into the Items column, it gets moved into the
                                -- Items group. But we want the Template to also remain in its original position
                                -- in the Templates group. So we add it back to its original position in the list.
                                (cards |> List.take dragOrigin.index)
                                    ++ (dragOrigin.card :: (cards |> List.drop dragOrigin.index))

                              else
                                cards
                            )

                        else
                            ( [], cards )
                in
                Html.div (containerStyles ++ dropEvents)
                    (cards_
                        |> List.indexedMap (eventfulCardView model offset index)
                    )

            _ ->
                Html.div containerStyles
                    (cards
                        |> List.indexedMap (eventfulCardView model offset index)
                    )
        ]


eventfulCardView : Model -> Int -> Int -> Int -> Card -> Html.Html Msg
eventfulCardView model offset columnIndex localIndex { cardType, description } =
    let
        globalIndex : Int
        globalIndex =
            offset + localIndex

        cardId : String
        cardId =
            "card-" ++ String.fromInt globalIndex
    in
    case ( cardSystem.info model.cardDnD, maybeDragCard model ) of
        ( Just { dragIndex }, Just dragCard ) ->
            let
                dropAttributes =
                    if columnIndex == 1 then
                        cardSystem.dropEvents globalIndex cardId

                    else
                        []
            in
            if description == "" && cardType /= dragCard.cardType then
                Html.div
                    (Html.Attributes.id cardId :: auxiliaryCardStyles ++ dropAttributes)
                    []

            else if description == "" && cardType == dragCard.cardType then
                Html.div
                    (Html.Attributes.id cardId :: auxiliaryCardStyles)
                    []

            else if globalIndex /= dragIndex then
                Html.div
                    (Html.Attributes.id cardId :: cardStyles yellow ++ dropAttributes)
                    [ Html.text description ]

            else if dragCard.cardType == Template then
                Html.div
                    (Html.Attributes.id cardId :: cardStyles yellow)
                    [ Html.text description ]

            else if (model.maybeDragOrigin |> Maybe.map (.card >> .cardType)) == Just Template then
                Html.div
                    (Html.Attributes.id cardId :: cardStyles green)
                    [ Html.text <| "Create a copy of " ++ description ]

            else
                Html.div
                    (Html.Attributes.id cardId :: cardStyles gray)
                    []

        _ ->
            if description == "" then
                Html.div
                    (Html.Attributes.id cardId :: auxiliaryCardStyles)
                    []

            else
                Html.div
                    (Html.Attributes.id cardId
                        :: cardStyles yellow
                        ++ cursorStyles
                        ++ cardSystem.dragEvents globalIndex cardId
                    )
                    [ Html.text description ]


cardGhostView : Model -> Html.Html Msg
cardGhostView model =
    case maybeDragCard model of
        Just { description } ->
            Html.div
                (cardStyles yellow ++ cursorStyles ++ cardSystem.ghostStyles model.cardDnD)
                [ Html.text description ]

        _ ->
            Html.text ""



-- HELPERS


gatherByCardType : List Card -> List (List Card)
gatherByCardType cards =
    List.foldr
        (\x acc ->
            case acc of
                [] ->
                    [ [ x ] ]

                (y :: restOfGroup) :: groups ->
                    if x.cardType == y.cardType then
                        (x :: y :: restOfGroup) :: groups

                    else
                        [ x ] :: acc

                [] :: _ ->
                    acc
        )
        []
        cards


maybeDragCard : Model -> Maybe Card
maybeDragCard { cardDnD, cards } =
    cardSystem.info cardDnD
        |> Maybe.andThen (\{ dragIndex } -> cards |> List.drop dragIndex |> List.head)


type alias Heading =
    { title : String
    , color : String
    }


getCardTypeHeading : List Card -> Heading
getCardTypeHeading cards =
    case cards of
        [] ->
            Heading "" ""

        card :: _ ->
            case card.cardType of
                Template ->
                    Heading "Templates" blue

                _ ->
                    Heading "Items" green



-- COLORS


yellow : String
yellow =
    "#ffdf76"


blue : String
blue =
    "#055b8f"


red : String
red =
    "#8f055b"


green : String
green =
    "#5b8f05"


gray : String
gray =
    "#a5a5a5"



-- STYLES


boardStyles : List (Html.Attribute msg)
boardStyles =
    [ Html.Attributes.style "display" "flex"
    , Html.Attributes.style "flex-wrap" "wrap"
    , Html.Attributes.style "justify-content" "center"
    , Html.Attributes.style "margin" "0 auto"
    , Html.Attributes.style "min-height" "600px"
    , Html.Attributes.style "padding" "2em 0"
    ]


columnStyles : String -> List (Html.Attribute msg)
columnStyles color =
    [ Html.Attributes.style "background-color" color
    , Html.Attributes.style "box-shadow" "0 0 0 1px black"
    , Html.Attributes.style "width" "220px"
    ]


columnHeadingStyles : String -> List (Html.Attribute msg)
columnHeadingStyles color =
    [ Html.Attributes.style "background-color" color
    , Html.Attributes.style "color" "white"
    , Html.Attributes.style "cursor" "pointer"
    , Html.Attributes.style "height" "60px"
    , Html.Attributes.style "margin" "0"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "align-items" "center"
    , Html.Attributes.style "justify-content" "center"
    ]


containerStyles : List (Html.Attribute msg)
containerStyles =
    [ Html.Attributes.style "background-color" "#999999"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "flex-direction" "column"
    , Html.Attributes.style "align-items" "center"
    , Html.Attributes.style "height" "calc(100% - 80px)"
    , Html.Attributes.style "padding-top" "20px"
    ]


cardStyles : String -> List (Html.Attribute msg)
cardStyles color =
    [ Html.Attributes.style "background-color" color
    , Html.Attributes.style "color" "black"
    , Html.Attributes.style "cursor" "pointer"
    , Html.Attributes.style "display" "flex"
    , Html.Attributes.style "align-items" "center"
    , Html.Attributes.style "justify-content" "center"
    , Html.Attributes.style "margin-bottom" "20px"
    , Html.Attributes.style "width" "170px"
    , Html.Attributes.style "height" "60px"
    ]


auxiliaryCardStyles : List (Html.Attribute msg)
auxiliaryCardStyles =
    [ Html.Attributes.style "background-color" "transparent"

    -- , Html.Attributes.style "box-shadow" "0 0 0 1px red"
    , Html.Attributes.style "flex-grow" "1"
    , Html.Attributes.style "width" "220px"
    , Html.Attributes.style "height" "auto"
    , Html.Attributes.style "min-height" "60px"
    ]


cursorStyles : List (Html.Attribute msg)
cursorStyles =
    [ Html.Attributes.style "cursor" "pointer" ]
