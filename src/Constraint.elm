module Constraint exposing (Model, focusOn, init, positions, unfocus, updateIdealPositions)

import Dict exposing (Dict)


{-| Lay out comments according to these constraints:

1.  if there is a focused comment, it must be at it's ideal position
2.  comments may not overlap
3.  comments should be as close as possible to their ideal position (but the
    first comment in an otherwise-overlapping sequence should be at it's ideal position)

Notably missing here: the ability to add new comments. That's just not something
that we need to solve for yet. Once we do, it should not be too hard to add
(recalculate the `comments` ordering with respect to the new height and ideal
position data, and re-solve.)

-}
type Model
    = Model
        { -- comment ID to dimensions. This needs to be sorted by idealPosition
          -- since this algorithm will be walking the data that way several
          -- times.
          comments : List ( Int, { height : Float, idealPosition : Float } )

        -- comment ID to actual position
        , positions : Dict Int Float

        -- margin, in pixels, to leave around comments
        , margin : Float

        -- is a comment selected? Which one?
        , focus : Maybe Int
        }


init :
    { heights : Dict Int Float
    , idealPositions : Dict Int Float
    , margin : Float
    }
    -> Model
init { heights, idealPositions, margin } =
    Model
        { comments =
            -- take the intersection of the heights and ideal positions. Gotta have both for a comment to do this algorithm!
            Dict.merge
                (\_ _ result -> result)
                (\key height idealPosition -> Dict.insert key { height = height, idealPosition = idealPosition })
                (\_ _ result -> result)
                heights
                idealPositions
                Dict.empty
                |> Dict.toList
                |> List.sortBy (\( _, { idealPosition } ) -> idealPosition)
        , positions = Dict.empty
        , margin = margin
        , focus = Nothing
        }
        |> solve


solve : Model -> Model
solve ((Model { focus }) as model) =
    if focus == Nothing then
        solveWithoutFocus model

    else
        solveWithFocus model


solveWithoutFocus : Model -> Model
solveWithoutFocus (Model guts) =
    Model
        { guts
            | positions =
                guts.comments
                    |> List.foldl
                        (\( id, { idealPosition, height } ) ( finalPositions, progressLine ) ->
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


solveWithFocus : Model -> Model
solveWithFocus (Model guts) =
    case guts.focus of
        Just id ->
            let
                (Model newGuts) =
                    solveWithoutFocus (Model guts)

                ( goUp, goDown ) =
                    -- == 0 is a trick to get the compiler to generate
                    -- more efficient code. Will not always be needed!
                    splitAtReversing (\( curId, _ ) -> curId - id == 0) newGuts.comments

                ( downwardPositions, _ ) =
                    List.foldl
                        (\( curId, { height, idealPosition } ) ( finalPositions, progressLine ) ->
                            if idealPosition >= progressLine then
                                ( Dict.insert curId idealPosition finalPositions
                                , idealPosition + height + newGuts.margin
                                )

                            else
                                ( Dict.insert curId progressLine finalPositions
                                , progressLine + height + newGuts.margin
                                )
                        )
                        ( newGuts.positions, 0 )
                        goDown

                ( downwardAndUpwardPositions, _ ) =
                    List.foldl
                        (\( curId, { height, idealPosition } ) ( finalPositions, progressLine ) ->
                            let
                                currentPosition =
                                    Dict.get curId newGuts.positions |> Maybe.withDefault idealPosition
                            in
                            if currentPosition + height + newGuts.margin <= progressLine then
                                ( finalPositions
                                , currentPosition
                                )

                            else
                                let
                                    finalPosition =
                                        currentPosition - (currentPosition + height + newGuts.margin - progressLine)
                                in
                                ( Dict.insert curId finalPosition finalPositions
                                , finalPosition
                                )
                        )
                        ( downwardPositions
                        , case goDown of
                            ( _, { idealPosition } ) :: _ ->
                                idealPosition

                            _ ->
                                -- infinity
                                1 / 0
                        )
                        goUp
            in
            Model { guts | positions = downwardAndUpwardPositions }

        Nothing ->
            solveWithFocus (Model guts)


updateIdealPositions : Dict Int Float -> Model -> Model
updateIdealPositions attachments (Model guts) =
    Model
        { guts
            | comments =
                guts.comments
                    |> List.filterMap
                        (\( id, metrics ) ->
                            Maybe.map
                                (\newIdealPosition -> ( id, { metrics | idealPosition = newIdealPosition } ))
                                (Dict.get id attachments)
                        )
                    |> List.sortBy (\( _, { idealPosition } ) -> idealPosition)
        }
        |> solve


focusOn : Int -> Model -> Model
focusOn id (Model guts) =
    Model { guts | focus = Just id } |> solve


unfocus : Model -> Model
unfocus (Model guts) =
    Model { guts | focus = Nothing } |> solve


positions : Model -> Dict Int Float
positions (Model guts) =
    guts.positions



-- utility


{-| Split a list at the first item that passes the test.

In the return value, the left list will be reversed and the right will be
forward. The matching element (if any) will be the first item of the right
list.

-}
splitAtReversing : (a -> Bool) -> List a -> ( List a, List a )
splitAtReversing test list =
    splitAtReversingHelp test list []


{-| internal function so Elm can do TCO here
-}
splitAtReversingHelp : (a -> Bool) -> List a -> List a -> ( List a, List a )
splitAtReversingHelp test list acc =
    case list of
        a :: rest ->
            if test a then
                ( acc, list )

            else
                splitAtReversingHelp test rest (a :: acc)

        [] ->
            ( acc, list )
