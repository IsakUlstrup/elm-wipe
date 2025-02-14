module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Html exposing (Attribute, Html, main_)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Position exposing (Direction(..), Position)
import Wipe exposing (PositionState)



-- CONSTANTS


slideDuration : Int
slideDuration =
    900



-- MODEL


type alias Model =
    PositionState


init : () -> ( Model, Cmd Msg )
init _ =
    ( Wipe.Still Position.zero
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedMove Direction
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedMove direction ->
            ( Wipe.move slideDuration direction model
            , Cmd.none
            )

        Tick dt ->
            ( Wipe.tick dt model
            , Cmd.none
            )



-- VIEW


viewPosition : Position -> Html msg
viewPosition ( x, y, z ) =
    Html.h3 []
        [ Html.text
            ("("
                ++ String.fromInt x
                ++ ", "
                ++ String.fromInt y
                ++ ", "
                ++ String.fromInt z
                ++ ")"
            )
        ]


viewTile : List (Attribute Msg) -> Position -> Html Msg
viewTile attrs position =
    Html.div (Html.Attributes.class "tile" :: attrs)
        [ viewPosition position
        , Html.div [ Html.Attributes.class "movement-buttons" ]
            [ Html.div [] []
            , Html.button [ onClick (ClickedMove North) ] [ Html.text "North" ]
            , Html.div [] []
            , Html.button [ onClick (ClickedMove West) ] [ Html.text "West" ]
            , Html.div [] []
            , Html.button [ onClick (ClickedMove East) ] [ Html.text "East" ]
            , Html.div [] []
            , Html.button [ onClick (ClickedMove South) ] [ Html.text "South" ]
            , Html.div [] []
            , Html.button [ onClick (ClickedMove Up) ] [ Html.text "Up" ]
            , Html.button [ onClick (ClickedMove Down) ] [ Html.text "Down" ]
            , Html.button [ onClick (ClickedMove (Warp ( 0, 0, 0 ))) ] [ Html.text "Warp home" ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        backgroundHue : Position -> Int
        backgroundHue ( x, y, _ ) =
            x * y
    in
    main_
        [ Html.Attributes.id "app"
        , Html.Attributes.style "background" ("hsl(" ++ String.fromInt (backgroundHue (Wipe.currenPosition model)) ++ ", 75%, 75%)")
        ]
        [ Wipe.view slideDuration (viewTile []) model ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown directionDecoder
        ]


directionDecoder : Decoder Msg
directionDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "w" ->
                        Decode.succeed (ClickedMove North)

                    "ArrowUp" ->
                        Decode.succeed (ClickedMove North)

                    "s" ->
                        Decode.succeed (ClickedMove South)

                    "ArrowDown" ->
                        Decode.succeed (ClickedMove South)

                    "a" ->
                        Decode.succeed (ClickedMove West)

                    "ArrowLeft" ->
                        Decode.succeed (ClickedMove West)

                    "d" ->
                        Decode.succeed (ClickedMove East)

                    "ArrowRight" ->
                        Decode.succeed (ClickedMove East)

                    _ ->
                        Decode.fail "unknown key"
            )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
