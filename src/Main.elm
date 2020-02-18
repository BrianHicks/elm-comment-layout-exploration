module Main exposing (..)

import Attachment exposing (Attachment)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Comment exposing (Comment)
import Constraint
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events
import Json.Decode as Decode
import Task exposing (Task)


type alias Model =
    { attachments : Dict Int Attachment
    , comments : Dict Int Comment
    , dragging : Maybe Int
    , commentPositions : Maybe Constraint.Model
    , focused : Maybe Int
    }


type Msg
    = MouseDownOnAttachment Int
    | MouseUp
    | MouseMove Float
    | AttachmentsMoved (List ( Int, Float ))
    | SetUpCommentConstraints ( List ( Int, Float ), List ( Int, Float ) )
    | FocusOn Int
    | Unfocus


init : () -> ( Model, Cmd Msg )
init _ =
    let
        attachments =
            [ Attachment 1 200 1
            , Attachment 2 220 2
            , Attachment 3 400 3
            ]

        comments =
            [ Comment 1 180
            , Comment 2 120
            , Comment 3 100
            ]
    in
    ( { attachments =
            attachments
                |> List.map (\({ id } as attachment) -> ( id, attachment ))
                |> Dict.fromList
      , comments =
            comments
                |> List.map (\({ id } as comment) -> ( id, comment ))
                |> Dict.fromList
      , dragging = Nothing
      , commentPositions = Nothing
      , focused = Nothing
      }
    , Task.map2 Tuple.pair
        (findNewAttachmentTopsTask attachments)
        (findCommentHeightsTask comments)
        |> Task.perform SetUpCommentConstraints
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDownOnAttachment id ->
            ( { model | dragging = Just id }, Cmd.none )

        MouseUp ->
            ( { model | dragging = Nothing }, Cmd.none )

        MouseMove top ->
            case model.dragging of
                Just id ->
                    ( { model
                        | attachments =
                            Dict.update
                                id
                                (Maybe.map (\attachment -> { attachment | top = top }))
                                model.attachments
                      }
                    , findNewAttachmentTops (Dict.values model.attachments)
                    )

                Nothing ->
                    ( model, Cmd.none )

        AttachmentsMoved attachments ->
            ( { model
                | commentPositions =
                    Maybe.map
                        (Constraint.updateAttachments (finalAttachments attachments))
                        model.commentPositions
              }
            , Cmd.none
            )

        SetUpCommentConstraints ( attachments, commentHeights ) ->
            ( { model
                | commentPositions =
                    Just <|
                        Constraint.init
                            { heights = Dict.fromList commentHeights
                            , attachments = finalAttachments attachments
                            , margin = 10
                            }
              }
            , Cmd.none
            )

        FocusOn commentId ->
            ( { model
                | focused = Just commentId
                , commentPositions = Maybe.map (Constraint.focusOn commentId) model.commentPositions
              }
            , Cmd.none
            )

        Unfocus ->
            ( { model
                | focused = Nothing
                , commentPositions = Maybe.map Constraint.unfocus model.commentPositions
              }
            , Cmd.none
            )


finalAttachments : List ( Int, Float ) -> Dict Int Float
finalAttachments =
    List.foldl
        (\( commentId, top ) soFar ->
            Dict.update commentId
                (\current ->
                    case current of
                        Nothing ->
                            Just top

                        Just otherTop ->
                            Just (min top otherTop)
                )
                soFar
        )
        Dict.empty


findNewAttachmentTopsTask : List Attachment -> Task Never (List ( Int, Float ))
findNewAttachmentTopsTask attachments =
    -- TODO: this may need Process.sleep 0 to be accurate in all cases
    attachments
        |> List.map
            (\{ id, commentId } ->
                Dom.getElement ("attachment-" ++ String.fromInt id)
                    |> Task.map (\{ element } -> Just ( commentId, element.y ))
                    |> Task.onError (\_ -> Task.succeed Nothing)
            )
        |> Task.sequence
        |> Task.map (List.filterMap identity)


findNewAttachmentTops : List Attachment -> Cmd Msg
findNewAttachmentTops attachments =
    Task.perform AttachmentsMoved (findNewAttachmentTopsTask attachments)


findCommentHeightsTask : List Comment -> Task Never (List ( Int, Float ))
findCommentHeightsTask comments =
    -- TODO: this may need Process.sleep 0 to be accurate in all cases
    comments
        |> List.map
            (\{ id } ->
                Dom.getElement ("comment-" ++ String.fromInt id)
                    |> Task.map (\{ element } -> Just ( id, element.height ))
                    |> Task.onError (\_ -> Task.succeed Nothing)
            )
        |> Task.sequence
        |> Task.map (List.filterMap identity)


view : Model -> Browser.Document Msg
view model =
    { title = "Comment Constraint Experiment"
    , body =
        [ Html.node "style" [] [ Html.text "* { user-select: none; -moz-user-select: none; -webkit-user-select: none; }" ]
        , Html.main_
            [ Attrs.style "width" "100%"
            , Attrs.style "height" "100vh"
            , Attrs.style "background-color" "aliceblue"
            ]
            (let
                horizMargin =
                    50
             in
             -- line
             [ Html.div
                [ Attrs.style "width" "1px"
                , Attrs.style "height" "94vh"
                , Attrs.style "position" "absolute"
                , Attrs.style "left" (String.fromInt horizMargin ++ "px")
                , Attrs.style "top" "3vh"
                , Attrs.style "background-color" "blue"
                ]
                []
             ]
                -- attachments
                ++ List.map
                    (\( id, attachment ) ->
                        Html.div
                            [ Attrs.id ("attachment-" ++ String.fromInt id)

                            -- events
                            , Html.Events.onMouseDown (MouseDownOnAttachment id)

                            -- position
                            , Attrs.style "position" "absolute"
                            , Attrs.style "left" (String.fromInt horizMargin ++ "px")
                            , Attrs.style "top" (String.fromFloat attachment.top ++ "px")
                            ]
                            [ Attachment.view attachment
                            ]
                    )
                    (Dict.toList model.attachments)
                -- comments
                ++ List.map
                    (\( id, comment ) ->
                        Html.div
                            [ Attrs.id ("comment-" ++ String.fromInt id)

                            -- events
                            , if model.focused == Just id then
                                Html.Events.onClick Unfocus

                              else
                                Html.Events.onClick (FocusOn id)

                            -- position
                            , Attrs.style "transition" "top 0.25s ease, left 0.25s ease"
                            , Attrs.style "position" "absolute"
                            , if model.focused == Just id then
                                Attrs.style "left" (String.fromInt (horizMargin * 2 - horizMargin // 2) ++ "px")

                              else
                                Attrs.style "left" (String.fromInt (horizMargin * 2) ++ "px")
                            , model.commentPositions
                                |> Maybe.map Constraint.positions
                                |> Maybe.andThen (Dict.get id)
                                -- render way off the screen so we can still get heights
                                |> Maybe.withDefault -9999
                                |> (\top -> String.fromFloat top ++ "px")
                                |> Attrs.style "top"
                            ]
                            [ Comment.view comment ]
                    )
                    (Dict.toList model.comments)
            )
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \{ dragging } ->
                Sub.batch
                    [ Browser.Events.onMouseUp (Decode.succeed MouseUp)
                    , if dragging /= Nothing then
                        Browser.Events.onMouseMove (Decode.map MouseMove (Decode.field "pageY" Decode.float))

                      else
                        Sub.none
                    ]
        }
