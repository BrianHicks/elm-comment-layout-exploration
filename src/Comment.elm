module Comment exposing (Comment, view)

import Html exposing (Html)
import Html.Attributes as Attrs


type alias Comment =
    { id : Int
    , height : Int
    }


view : Comment -> Html msg
view { height, id } =
    Html.div
        [ Attrs.style "width" "150px"
        , Attrs.style "height" (String.fromInt height ++ "px")
        , Attrs.style "border" "1px solid blue"
        , Attrs.style "border-radius" "10px"
        , Attrs.style "background-color" "white"
        ]
        [ Html.text (String.fromInt id) ]
