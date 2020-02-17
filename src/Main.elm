module Main exposing (..)

import Attachment exposing (Attachment)
import Browser
import Browser.Events
import Comment exposing (Comment)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events
import Json.Decode as Decode


type alias Model =
    { attachments : Dict Int Attachment
    , comments : List Comment
    , dragging : Maybe Int
    }


type Msg
    = MouseDownOnAttachment Int
    | MouseUp
    | MouseMove Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { attachments =
            Dict.fromList
                [ ( 1, Attachment 20 )
                , ( 2, Attachment 40 )
                , ( 3, Attachment 100 )
                ]
      , comments = []
      , dragging = Nothing
      }
    , Cmd.none
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
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


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
                            [ Html.Events.onMouseDown (MouseDownOnAttachment id)

                            -- position
                            , Attrs.style "position" "absolute"
                            , Attrs.style "left" (String.fromInt horizMargin ++ "px")
                            , Attrs.style "top" (String.fromInt attachment.top ++ "px")
                            ]
                            [ Attachment.view attachment
                            ]
                    )
                    (Dict.toList model.attachments)
                -- comments
                ++ List.map Comment.view model.comments
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
                        Browser.Events.onMouseMove (Decode.map MouseMove (Decode.field "pageY" Decode.int))

                      else
                        Sub.none
                    ]
        }
