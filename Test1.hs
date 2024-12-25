{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
--problem1
class IDict d k v where
  insert       :: k -> v -> d k v -> d k v
  maybeGet     :: k -> d k v -> Maybe v
  getOrDefault :: k -> d k v -> v -> v
  contains     :: k -> d k v -> Bool
  delete       :: k -> d k v -> d k v
  elems        :: d k v -> [v]
  keys         :: d k v -> [k]
  size         :: d k v -> Int
  empty        :: d k v
--endproblem1
--problem2
data Dict k v = Dict [(k, v)]

instance (Eq k, Eq v) => IDict Dict k v where
  insert k v (Dict xs) = Dict $ (k,v) : xs
  
  maybeGet key (Dict list) = lookup key list
  
  getOrDefault k d v = maybe v id (maybeGet k d)
  
  contains key (Dict list) = any (\(k, v) -> k == key) list
  
  delete key (Dict list) = Dict (filter ((/= key) . fst) list)
  
  elems (Dict list) = map snd list
  
  keys (Dict list) = map fst list
  
  size (Dict list) = length list
  
  empty = Dict []
--endproblem2
--problem3
instance (Show k, Show v) => Show (Dict k v) where
  show (Dict []) = "{}"
  show (Dict xs) = "{" ++ go xs ++ "}"
    where
      go [] = ""
      go [(k, v)] = show k ++ " : " ++ show v
      go ((k, v):xs) = show k ++ " : " ++ show v ++ ", " ++ go xs

--endproblem3


main = do
  let kvPairs = [(1,'h'), (2,'e'), (3, 'l'), (4,'l'), (5, 'o')]
      dict = fromPairs kvPairs
  print dict
    where
      fromPairs :: (Eq k, Eq v) => [(k, v)] -> Dict k v
      fromPairs = foldl insert' empty
      insert' dict (k, v) = insert k v dict
