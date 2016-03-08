module Trigger where

import Entity
import GameState as GS
import Prism

type alias Msg = ()

handleMsg : Msg -> GS.EntityUpdate m
handleMsg _ = GS.pureUpdate <| \ _ e -> e

find : (Int, Int) -> GS.GameState -> List GS.Id
find pos = GS.findEntities ((==) (Just pos) << Prism.get Entity.trigger)
