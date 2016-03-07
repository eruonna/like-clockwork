module Movement where

import Entity exposing (Entity)
import GameState as GS
import Map
import Prism

type Direction =
  N | S | E | W | NE | NW | SE | SW

move : Direction -> (Int, Int) -> (Int, Int)
move dir (x, y) = case dir of
  N -> (x, y-1)
  S -> (x, y+1)
  E -> (x+1, y)
  W -> (x-1, y)
  NE -> move N <| move E (x,y)
  NW -> move N <| move W (x,y)
  SE -> move S <| move E (x,y)
  SW -> move S <| move W (x,y)

tryMove : Direction -> GS.EntityUpdate
tryMove dir gs e = case Prism.get Entity.pos e of
  Just p -> let p' = move dir p
            in if Map.canMove p' gs.map then Prism.set Entity.pos p' e else e
  Nothing -> e
