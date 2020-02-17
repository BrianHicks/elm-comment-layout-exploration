module Attachment exposing (Attachment, init, view)

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events


type alias Attachment =
    { top : Int }


init : Int -> Attachment
init =
    Attachment << abs


view : Attachment -> Html msg
view _ =
    Html.div
        [ Attrs.style "width" "15px"
        , Attrs.style "height" "15px"
        , Attrs.style "border" "1px solid blue"
        , Attrs.style "border-radius" "100%"
        , Attrs.style "background-color" "white"
        , Attrs.style "cursor" "pointer"
        ]
        []
