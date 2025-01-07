module Position exposing (Direction(..), Position, directionToSoString, move, zero)


type alias Position =
    ( Int, Int, Int )



-- TODO: Add Warp Position


type Direction
    = North
    | South
    | West
    | East
    | Up
    | Down
    | Warp Position


directionToSoString : Direction -> String
directionToSoString direction =
    case direction of
        North ->
            "north"

        South ->
            "south"

        West ->
            "west"

        East ->
            "east"

        Up ->
            "up"

        Down ->
            "down"

        Warp _ ->
            "warp"


zero : Position
zero =
    ( 0, 0, 0 )


north : Position
north =
    ( 0, -1, 0 )


south : Position
south =
    ( 0, 1, 0 )


east : Position
east =
    ( 1, 0, 0 )


west : Position
west =
    ( -1, 0, 0 )


up : Position
up =
    ( 0, 0, 1 )


down : Position
down =
    ( 0, 0, -1 )


add : Position -> Position -> Position
add ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + x2, y1 + y2, z1 + z2 )


move : Direction -> Position -> Position
move direction position =
    case direction of
        North ->
            add north position

        South ->
            add south position

        West ->
            add west position

        East ->
            add east position

        Up ->
            add up position

        Down ->
            add down position

        Warp warpTo ->
            warpTo
