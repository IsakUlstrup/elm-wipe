module Wipe exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Position exposing (Direction, Position)


type PositionState
    = Moving Position Direction Int
    | Still Position


currenPosition : PositionState -> Position
currenPosition state =
    case state of
        Still position ->
            position

        Moving from _ _ ->
            from


tick : Float -> PositionState -> PositionState
tick dt state =
    case state of
        Still _ ->
            state

        Moving from direction cd ->
            if cd == 0 then
                Still (Position.move direction from)

            else
                Moving from direction (cd - round dt |> max 0)


move : Int -> Direction -> PositionState -> PositionState
move duration direction state =
    case state of
        Still position ->
            Moving position direction duration

        Moving _ _ _ ->
            state


view : Int -> (Position -> Html msg) -> PositionState -> Html msg
view duration viewPosition model =
    let
        currentDirection : String
        currentDirection =
            case model of
                Still _ ->
                    "none"

                Moving _ direction _ ->
                    Position.directionToSoString direction

        animationDuration : Attribute msg
        animationDuration =
            Html.Attributes.attribute "style" ("--slide-duration: " ++ String.fromInt duration ++ "ms")
    in
    Html.section
        [ Html.Attributes.class currentDirection
        , Html.Attributes.class "wipe-container"
        ]
        (case model of
            Still position ->
                [ Html.div [ Html.Attributes.class "wipe-panel", animationDuration ] [ viewPosition position ]
                ]

            Moving from direction _ ->
                [ Html.div [ Html.Attributes.class "wipe-panel", animationDuration ] [ viewPosition (Position.move direction from) ]
                , Html.div [ Html.Attributes.class "wipe-panel", animationDuration ] [ viewPosition from ]
                ]
        )
