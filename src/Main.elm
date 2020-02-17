module Main exposing (..)

import Attachment exposing (Attachment(..))
import Browser
import Browser.Dom as Dom
import Browser.Events
import Comment exposing (Comment)
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
    }


type Msg
    = MouseDownOnAttachment Int
    | MouseUp
    | MouseMove Float
    | AttachmentsMoved (List ( Int, Float ))


init : () -> ( Model, Cmd Msg )
init _ =
    let
        attachments =
            Dict.fromList
                [ ( 1, Attachment 200 1 )
                , ( 2, Attachment 220 2 )
                , ( 3, Attachment 400 3 )
                ]

        comments =
            Dict.fromList
                [ ( 1, Comment 180 )
                , ( 2, Comment 120 )
                , ( 3, Comment 100 )
                ]
    in
    ( { attachments = attachments
      , comments = comments
      , dragging = Nothing
      }
    , findNewAttachmentTops (Dict.keys attachments)
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
                                (Maybe.map (\(Attachment _ commentId) -> Attachment top commentId))
                                model.attachments
                      }
                    , findNewAttachmentTops (Dict.keys model.attachments)
                    )

                Nothing ->
                    ( model, Cmd.none )

        AttachmentsMoved tops ->
            ( model, Cmd.none )


findNewAttachmentTopsTask : List Int -> Task Never (List ( Int, Float ))
findNewAttachmentTopsTask ids =
    -- TODO: this may need Process.sleep 0 to be accurate in all cases
    ids
        |> List.map
            (\id ->
                Dom.getElement ("attachment-" ++ String.fromInt id)
                    |> Task.map (\{ element } -> Just ( id, (Debug.log "element" element).y ))
                    |> Task.onError (\_ -> Task.succeed Nothing)
            )
        |> Task.sequence
        |> Task.map (List.filterMap identity)


findNewAttachmentTops : List Int -> Cmd Msg
findNewAttachmentTops ids =
    Task.perform AttachmentsMoved (findNewAttachmentTopsTask ids)


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
                    (\( id, (Attachment top _) as attachment ) ->
                        Html.div
                            [ Attrs.id ("attachment-" ++ String.fromInt id)

                            -- events
                            , Html.Events.onMouseDown (MouseDownOnAttachment id)

                            -- position
                            , Attrs.style "position" "absolute"
                            , Attrs.style "left" (String.fromInt horizMargin ++ "px")
                            , Attrs.style "top" (String.fromFloat top ++ "px")
                            ]
                            [ Attachment.view attachment
                            ]
                    )
                    (Dict.toList model.attachments)
                -- comments
                ++ List.map
                    (\( id, comment ) ->
                        Html.div
                            [ Attrs.id ("comment-" ++ String.fromInt id) ]
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
