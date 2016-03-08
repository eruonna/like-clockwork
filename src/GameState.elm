module GameState where

import Dict exposing (Dict)

import Entity exposing (Entity)
import Map exposing (Map)
import Messaging exposing (Messaging)
import Prism exposing (Prism, prism)

import Games.Roguelike.Tile exposing (Tile)

type alias Id = Int

type alias GameState =
  { map : Map
  , entities : Dict Id Entity
  , nextId : Id
  }

empty : GameState
empty = { map = Map.empty
        , entities = Dict.empty
        , nextId = 0
        }

setMap : Map -> GameState -> GameState
setMap m gs = { gs | map = m }

entity : Id -> Prism GameState Entity
entity id = prism (Dict.get id << .entities)
                  (\ e gs -> { gs | entities = Dict.insert id e gs.entities })
                  (\ gs -> { gs | entities = Dict.remove id gs.entities })

addEntity : Entity -> GameState -> (GameState, Id)
addEntity e gs = ( { gs | entities = Dict.insert gs.nextId e gs.entities
                        , nextId = gs.nextId + 1 }
                 , gs.nextId
                 )

type alias EntityUpdate = GameState -> Entity -> Messaging Message Entity

pureUpdate : (GameState -> Entity -> Entity) -> EntityUpdate
pureUpdate u gs e = Messaging.return (u gs e)

runEntityUpdate : Id -> EntityUpdate -> GameState -> GameState
runEntityUpdate id eu gs = uncurry runMessages <| Prism.updateM (entity id) (eu gs) gs

noUpdate : EntityUpdate
noUpdate _ e = Messaging.return e

type Message =
  NoMessage

handleMessage : Message -> GameState -> Messaging Message GameState
handleMessage m gs = case m of
  NoMessage -> Messaging.return gs

runMessages : List Message -> GameState -> GameState
runMessages ms gs =
  if List.isEmpty ms
  then gs
  else uncurry runMessages <| Messaging.foldM handleMessage gs ms

runMessage : Message -> GameState -> GameState
runMessage m = runMessages [m]
