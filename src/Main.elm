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
    }


type Msg
    = MouseDownOnAttachment Int
    | MouseUp
    | MouseMove Float
    | AttachmentsMoved (List ( Int, Float ))
    | GotCommentHeights (List ( Int, Float ))


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
      }
    , Cmd.batch
        [ findNewAttachmentTops (List.map .id attachments)
        , findCommentHeights (List.map .id comments)
        ]
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
                    , findNewAttachmentTops (Dict.keys model.attachments)
                    )

                Nothing ->
                    ( model, Cmd.none )

        AttachmentsMoved tops ->
            ( model, Cmd.none )

        GotCommentHeights heights ->
            ( model, Cmd.none )


findNewAttachmentTopsTask : List Int -> Task Never (List ( Int, Float ))
findNewAttachmentTopsTask ids =
    -- TODO: this may need Process.sleep 0 to be accurate in all cases
    ids
        |> List.map
            (\id ->
                Dom.getElement ("attachment-" ++ String.fromInt id)
                    |> Task.map (\{ element } -> Just ( id, element.y ))
                    |> Task.onError (\_ -> Task.succeed Nothing)
            )
        |> Task.sequence
        |> Task.map (List.filterMap identity)


findNewAttachmentTops : List Int -> Cmd Msg
findNewAttachmentTops ids =
    Task.perform AttachmentsMoved (findNewAttachmentTopsTask ids)


findCommentHeightsTask : List Int -> Task Never (List ( Int, Float ))
findCommentHeightsTask ids =
    -- TODO: this may need Process.sleep 0 to be accurate in all cases
    ids
        |> List.map
            (\id ->
                Dom.getElement ("comment-" ++ String.fromInt id)
                    |> Task.map (\{ element } -> Just ( id, element.height ))
                    |> Task.onError (\_ -> Task.succeed Nothing)
            )
        |> Task.sequence
        |> Task.map (List.filterMap identity)


findCommentHeights : List Int -> Cmd Msg
findCommentHeights ids =
    Task.perform GotCommentHeights (findCommentHeightsTask ids)


view : Model -> Browser.Document Msg
view model =
    { title = "Comment Constraint Experiment"
    , body =
        [ Html.node "style" [] [ Html.text "body { user-select: none; }" ]
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
                ++ List.filterMap
                    (\( id, comment ) ->
                        model.commentPositions
                            |> Maybe.map Constraint.positions
                            |> Maybe.andThen (Dict.get id)
                            |> Maybe.map
                                (\top ->
                                    Html.div
                                        [ Attrs.id ("comment-" ++ String.fromInt id)

                                        -- position
                                        , Attrs.style "position" "absolute"
                                        , Attrs.style "left" (String.fromInt (horizMargin * 2) ++ "px")
                                        , Attrs.style "top" (String.fromFloat top ++ "px")
                                        , Attrs.style "transition" "top 0.5s ease"
                                        ]
                                        [ Comment.view comment ]
                                )
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
