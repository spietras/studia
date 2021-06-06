-- Map related code

module Map where

import           Types

mapKeys :: Map a b -> [a]
mapKeys []          = []
mapKeys ((k, v):xs) = k:(mapKeys xs)

mapLookup :: Eq a => Map a b -> a -> Maybe b
mapLookup [] _ = Nothing
mapLookup ((k, v):xs) key | key == k  = Just v
                          | otherwise = mapLookup xs key

mapKeyExists :: Eq a => Map a b -> a -> Bool
mapKeyExists map k = case (mapLookup map k) of Just _  -> True
                                               Nothing -> False

mapSet :: Eq a =>  Map a b -> a -> b -> Map a b
mapSet [] k v = [(k, v)]
mapSet ((k, v):xs) key value | key == k  = ((k, value):xs)
                             | otherwise = ((k,v):(mapSet xs key value))
