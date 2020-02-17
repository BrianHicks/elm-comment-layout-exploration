module Attachment exposing (Attachment, init, view)

import Html exposing (Html)
import Html.Attributes as Attrs


type alias Attachment =
    { top : Int }


init : Int -> Attachment
init =
    Attachment << abs


view : Int -> Attachment -> Html msg
view left { top } =
    Html.div
        [ Attrs.style "width" "15px"
        , Attrs.style "height" "15px"
        , Attrs.style "border" "1px solid blue"
        , Attrs.style "border-radius" "100%"
        , Attrs.style "background-color" "white"
        , Attrs.style "cursor" "pointer"

        -- position
        , Attrs.style "position" "absolute"
        , Attrs.style "left" (String.fromInt left ++ "px")
        , Attrs.style "top" (String.fromInt top ++ "px")
        ]
        []
