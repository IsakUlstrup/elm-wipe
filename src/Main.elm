module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Html exposing (Attribute, Html, main_)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Position exposing (Direction(..), Position)



-- POSITION STATE


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



-- MODEL


type alias Model =
    PositionState


init : () -> ( Model, Cmd Msg )
init _ =
    ( Still Position.zero, Cmd.none )



-- UPDATE


type Msg
    = ClickedMove Direction
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedMove direction ->
            ( case model of
                Still position ->
                    Moving position direction 300

                Moving _ _ _ ->
                    model
            , Cmd.none
            )

        Tick dt ->
            ( case model of
                Still _ ->
                    model

                Moving from direction cd ->
                    if cd == 0 then
                        Still (Position.move direction from)

                    else
                        Moving from direction (cd - round dt |> max 0)
            , Cmd.none
            )



-- VIEW


viewTile : List (Attribute Msg) -> Position -> Html Msg
viewTile attrs position =
    Html.div (Html.Attributes.class "tile" :: attrs)
        [ Html.h3 [] [ Html.text (Debug.toString position) ]
        , Html.button [ onClick (ClickedMove North) ] [ Html.text "North" ]
        , Html.button [ onClick (ClickedMove South) ] [ Html.text "South" ]
        , Html.button [ onClick (ClickedMove West) ] [ Html.text "West" ]
        , Html.button [ onClick (ClickedMove East) ] [ Html.text "East" ]
        ]


view : Model -> Html Msg
view model =
    let
        axis =
            case model of
                Still _ ->
                    "none"

                Moving _ North _ ->
                    "vertical"

                Moving _ South _ ->
                    "vertical"

                _ ->
                    "horizontal"

        backgroundHue ( x, y, _ ) =
            x * y
    in
    main_
        [ Html.Attributes.id "app"
        , Html.Attributes.class axis
        , Html.Attributes.style "background" ("hsl(" ++ String.fromInt (backgroundHue (currenPosition model)) ++ ", 75%, 75%)")
        ]
        (case model of
            Still position ->
                [ Html.section [] [ viewTile [] position ]
                ]

            Moving from North _ ->
                [ Html.section [ Html.Attributes.class "enter-vertical" ] [ viewTile [] (Position.move North from) ]
                , Html.section [ Html.Attributes.class "leave-vertical" ] [ viewTile [] from ]
                ]

            Moving from South _ ->
                [ Html.section [ Html.Attributes.class "leave-vertical" ] [ viewTile [] from ]
                , Html.section [ Html.Attributes.class "enter-vertical" ] [ viewTile [] (Position.move South from) ]
                ]

            Moving from West _ ->
                [ Html.section [ Html.Attributes.class "enter-horizontal" ] [ viewTile [] (Position.move West from) ]
                , Html.section [ Html.Attributes.class "leave-horizontal" ] [ viewTile [] from ]
                ]

            Moving from East _ ->
                [ Html.section [ Html.Attributes.class "leave-horizontal" ] [ viewTile [] from ]
                , Html.section [ Html.Attributes.class "enter-horizontal" ] [ viewTile [] (Position.move East from) ]
                ]
        )



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