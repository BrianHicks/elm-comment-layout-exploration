module Constraint exposing (Model, focusOn, init, positions, unfocus, updateAttachments)

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

        -- is a comment selected? Which one?
        , focus : Maybe Int
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


solveWithFocus : Model -> Model
solveWithFocus (Model guts) =
    case guts.focus of
        Just id ->
            let
                (Model newGuts) =
                    solveWithoutFocus (Model guts)

                ( goUp, goDown ) =
                    newGuts.attachments
                        |> Dict.toList
                        |> List.sortBy Tuple.second
                        -- == 0 is a trick to get the compiler to generate
                        -- more efficient code. Will not always be needed!
                        |> splitAtReversing (\( curId, _ ) -> curId - id == 0)

                ( downwardPositions, _ ) =
                    List.foldl
                        (\( curId, idealPosition ) ( finalPositions, progressLine ) ->
                            let
                                height =
                                    Dict.get curId newGuts.heights |> Maybe.withDefault 0
                            in
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
                        (\( curId, idealPosition ) ( finalPositions, progressLine ) ->
                            let
                                height =
                                    Dict.get curId newGuts.heights |> Maybe.withDefault 0

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
                            ( _, start ) :: _ ->
                                start

                            _ ->
                                -- infinity
                                1 / 0
                        )
                        goUp
            in
            Model { guts | positions = downwardAndUpwardPositions }

        Nothing ->
            solveWithFocus (Model guts)


updateAttachments : Dict Int Float -> Model -> Model
updateAttachments attachments (Model guts) =
    if attachments == guts.attachments then
        Model guts

    else
        Model { guts | attachments = attachments } |> solve


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
