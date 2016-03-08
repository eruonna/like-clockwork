module Movement where

import List

import Entity exposing (Entity)
import GameState as GS
import Map
import Messages exposing (Message)
import Messaging
import Prism
import Trigger

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

doMove : (Int, Int) -> GS.EntityUpdate (GS.Id, Message)
doMove p gs e =
  (Messaging.mapM (\ id -> Messaging.send (id, Messages.Trigger ()))
    <| List.map (Messaging.return) <| Trigger.find p gs)
  `Messaging.andThen` always (Messaging.return (Prism.set Entity.pos p e))

tryMove : Direction -> GS.EntityUpdate (GS.Id, Message)
tryMove dir = (\ gs e -> case Prism.get Entity.pos e of
  Just p -> let p' = move dir p
            in if Map.canMove p' gs.map
               then doMove p' gs e
               else Messaging.return e
  Nothing -> Messaging.return e)
