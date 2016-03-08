module Messaging where

import List

type alias Messaging a x = (List a, x)

send : a -> Messaging a ()
send msg = ([msg], ())

return : x -> Messaging a x
return x = ([], x)

andThen : Messaging a x -> (x -> Messaging a y) -> Messaging a y
andThen (l1, x) f =
  let (l2, y) = f x
  in (l1 ++ l2, y)

map : (x -> y) -> Messaging a x -> Messaging a y
map f (l, x) = (l, f x)

map2 : (x -> y -> z) -> Messaging a x -> Messaging a y -> Messaging a z
map2 f (l1, x) (l2, y) = (l1 ++ l2, f x y)

traverse : List (Messaging a x) -> Messaging a (List x)
traverse l = case l of
  [] -> return []
  m :: r -> map2 (::) m (traverse r)

mapM : (x -> Messaging a y) -> List (Messaging a x) -> Messaging a (List y)
mapM f ms = traverse <| List.map (\ x -> x `andThen` f) ms

foldM : (x -> y -> Messaging a y) -> y -> List x -> Messaging a y
foldM op z l = List.foldl (\ x my -> my `andThen` op x) (return z) l
