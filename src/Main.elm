module Main exposing (..)

import Attachment exposing (Attachment)
import Browser
import Comment exposing (Comment)
import Html exposing (Html)
import Html.Attributes as Attrs


type alias Model =
    { attachments : List Attachment
    , comments : List Comment
    }


type Msg
    = AddNewCommentAndAttachment


init : () -> ( Model, Cmd Msg )
init _ =
    ( { attachments = []
      , comments = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    Debug.todo "update"


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
                ++ List.map (Attachment.view horizMargin) model.attachments
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
        , subscriptions = \_ -> Sub.none
        }
