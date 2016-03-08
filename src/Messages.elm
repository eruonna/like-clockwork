module Messages where

import Entity exposing (Entity)
import GameState as GS
import Messaging exposing (Messaging)

type alias TriggerMsg = ()

type Message =
    Trigger TriggerMsg
  | NoMessage

handleMessage : (GS.Id, Message) -> GS.GameState
                    -> Messaging (GS.Id, Message) GS.GameState
handleMessage (id, msg) gs = case msg of
  Trigger tmsg -> GS.runEntityUpdate id (handleTriggerMsg tmsg) gs
  NoMessage -> Messaging.return gs

handleTriggerMsg : TriggerMsg -> GS.EntityUpdate (GS.Id, Message)
handleTriggerMsg _ = GS.noUpdate
