module Attachment exposing (Attachment, view)

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events


type alias Attachment =
    { id : Int
    , top : Float
    , commentId : Int
    }


view : Attachment -> Html msg
view { commentId } =
    Html.div
        [ Attrs.style "width" "15px"
        , Attrs.style "height" "15px"
        , Attrs.style "border" "1px solid blue"
        , Attrs.style "border-top-right-radius" "100%"
        , Attrs.style "border-bottom-right-radius" "100%"
        , Attrs.style "background-color" "white"
        , Attrs.style "cursor" "pointer"
        , Attrs.style "display" "flex"
        , Attrs.style "align-items" "center"
        , Attrs.style "justify-content" "center"
        ]
        [ Html.text (String.fromInt commentId) ]
