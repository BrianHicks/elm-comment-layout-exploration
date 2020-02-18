module Constraint exposing (Model, init, positions)

import Dict exposing (Dict)


type Model
    = Model
        { -- comment ID to comment height
          heights : Dict Int Float

        -- comment ID to ideal position
        , attachments : Dict Int Float

        -- comment ID to actual position
        , positions : Dict Int Float

        -- margin, in pixels, to leave around comments
        , margin : Int
        }


init :
    { heights : Dict Int Float
    , attachments : Dict Int Float
    , margin : Int
    }
    -> Model
init { heights, attachments, margin } =
    Model
        { heights = heights
        , attachments = attachments
        , positions = attachments
        , margin = margin
        }
        |> Debug.log "initial"


positions : Model -> Dict Int Float
positions (Model guts) =
    guts.positions
