module Constraint exposing (Model, init, positions, updateAttachments)

import Dict exposing (Dict)


type Model
    = Model
        { -- comment ID to comment height
          heights : Dict Int Float

        -- comment ID to ideal position
        , attachments : Dict Int Float

        -- comment ID to actual position
        , positions : Dict Int Float

        -- margin, in pixels, to leave around comments
        , margin : Float
        }


init :
    { heights : Dict Int Float
    , attachments : Dict Int Float
    , margin : Float
    }
    -> Model
init { heights, attachments, margin } =
    Model
        { heights = heights
        , attachments = attachments
        , positions = Dict.empty
        , margin = margin
        }
        |> solveWithoutFocus


solveWithoutFocus : Model -> Model
solveWithoutFocus (Model guts) =
    Model
        { guts
            | positions =
                guts.attachments
                    |> Dict.toList
                    |> List.sortBy Tuple.second
                    |> List.foldl
                        (\( id, idealPosition ) ( finalPositions, progressLine ) ->
                            let
                                height =
                                    Dict.get id guts.heights
                                        |> Maybe.withDefault 0
                            in
                            if idealPosition >= progressLine then
                                ( Dict.insert id idealPosition finalPositions
                                , idealPosition + height + guts.margin
                                )

                            else
                                ( Dict.insert id progressLine finalPositions
                                , progressLine + height + guts.margin
                                )
                        )
                        ( Dict.empty, 0 )
                    |> Tuple.first
        }


updateAttachments : Dict Int Float -> Model -> Model
updateAttachments attachments (Model guts) =
    if attachments == guts.attachments then
        Model guts

    else
        Model { guts | attachments = attachments } |> solveWithoutFocus


positions : Model -> Dict Int Float
positions (Model guts) =
    guts.positions
