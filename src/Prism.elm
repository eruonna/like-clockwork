module Prism ( Prism
             , prism
             , compose
             , get
             , set
             , clear
             , update
             , updateM
             ) where

import Maybe exposing (Maybe)

import Messaging exposing (Messaging)

type Prism a b = Prism
  { getter : a -> Maybe b
  , setter : b -> a -> a
  , clearer : a -> a
  }

prism : (a -> Maybe b) -> (b -> a -> a) -> (a -> a) -> Prism a b
prism get set clear = Prism
  { getter = get
  , setter = set
  , clearer = clear
  }

compose : Prism a b -> Prism b c -> Prism a c
compose (Prism p1) (Prism p2) = Prism
  { getter = \a -> p1.getter a `Maybe.andThen` p2.getter
  , setter = \b -> update (Prism p1) (update (Prism p2) (always b))
  , clearer = update (Prism p1) p2.clearer
  }

get : Prism a b -> a -> Maybe b
get (Prism p) = p.getter

set : Prism a b -> b -> a -> a
set (Prism p) = p.setter

clear : Prism a b -> a -> a
clear (Prism p) = p.clearer

update : Prism a b -> (b -> b) -> a -> a
update p u a = case get p a of
  Nothing -> a
  Just b -> set p (u b) a

updateM : Prism a b -> (b -> Messaging m b) -> a -> Messaging m a
updateM p u a = case get p a of
  Nothing -> Messaging.return a
  Just b -> Messaging.map (\ b' -> set p b' a) <| u b
