module Heap where

-- memory consists of words addressed by word.
-- use a translation to retrieve individual bytes or address by byte
-- how a string is packed and encoded in memory isn't determined.
-- use library functions to access the characters of a string.

import Data.IntMap as IM hiding (map, foldl)

type Heap = IntMap Int

empty :: Heap
empty  = IM.empty

peek :: Heap -> Int -> Int
peek h i = IM.findWithDefault 0 i h

peekMaybe :: Heap -> Int -> Maybe Int
peekMaybe h i = IM.lookup i h

poke :: Int -> Int -> Heap -> Heap
poke = IM.insert 

dump :: Heap -> [(Int,Int)]
dump = IM.toList

load :: [(Int,Int)] -> Heap
load = IM.fromList

merge :: Heap -> Heap -> Heap
merge = IM.union

memcpy :: Int -> [Int] -> Heap -> Heap
memcpy base ws h = foldl (\h (x,i) -> IM.insert i x h) h (zip ws [base..])
