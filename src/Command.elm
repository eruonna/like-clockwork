module Command where

import GameState as GS
import Messages exposing (Message)
import Movement exposing (Direction)

type Command =
    Move Direction
  | Unknown

run : Command -> GS.EntityUpdate (GS.Id, Message)
run cmd = case cmd of
  Move dir -> Movement.tryMove dir
  Unknown -> GS.noUpdate
