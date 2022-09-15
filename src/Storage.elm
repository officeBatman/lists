port module Storage exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


port set : Value -> Cmd msg

