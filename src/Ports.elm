port module Ports exposing (storeState)

import Json.Encode


port storeState : Json.Encode.Value -> Cmd msg
