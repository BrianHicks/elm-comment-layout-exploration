module Comment exposing (Comment, init, view)

import Html exposing (Html)
import Html.Attributes as Attrs


type alias Comment =
    { height : Int }


init : Int -> Comment
init =
    Comment << abs


view : Comment -> Html msg
view { height } =
    Html.div
        [ Attrs.style "width" "150px"
        , Attrs.style "height" (String.fromInt height ++ "px")
        , Attrs.style "border" "1px solid blue"
        , Attrs.style "border-radius" "10px"
        , Attrs.style "background-color" "white"
        ]
        []