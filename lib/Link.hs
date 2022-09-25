module Link where

import Data.Map (Map, (!))
import qualified Data.Map as M

import PackedString
import Heap
import Compile

data Borax = Borax
  { bxHeap :: Heap
  , bxFuncs :: Map Int Func
  , bxNames :: Map String Int
  , bxStrings :: Map String Int
  } deriving Show

data GlobalType = GloAtom | GloVec | GloFun
  deriving Show

data GlobalEntity a = GlobalEntity
  { gloName :: String
  , gloSize :: Int
  , gloType :: GlobalType
  , gloInit :: [a] }
      deriving Show

type GlobalEnv a = Map String (GlobalEntity a)

link :: [Borate LinkMe] -> Either String Borax
link bors = do
  let strings = borateStrings bors
  let (base1, stringLocs, heap1) = compileStringPlacement 100 strings

  let rawExt = concatMap borateRawExtrns bors
  genvNeedsLink <- dedupExtrns rawExt
  let (_, locs) = locateEntities base1 genvNeedsLink

  -- disallow cycles in initializers
  -- disallow missing entities (dangling reference in initializer or functions)

  let funcs = concatMap borFuncs bors
  let funcByAddr = M.fromList (map (\fun -> (locs M.! funName fun, fun)) funcs)

  let entNames = M.keys locs
  let genv :: Map String (GlobalEntity Int)
      genv = fmap (\glo -> glo { gloInit = map (resolveInitializer genvNeedsLink stringLocs locs) (gloInit glo)}) genvNeedsLink

  let heap = foldl (\h name -> putGlobal (locs ! name) (genv ! name) h) heap1 entNames
  return (Borax heap funcByAddr locs stringLocs)


-- lay out global entities in address space, based on first argument.
locateEntities :: Int -> GlobalEnv a -> (Int, Map String Int)
locateEntities base genv = foldl f (base, M.empty) (M.toList genv) where
  f (ptr,out) (name, ent) = (ptr+gloSize ent, M.insert name ptr out)


-- this blindly traverses the global env looking for a value. 
-- freezes if there is a cycle
resolveInitializer :: GlobalEnv LinkMe -> Map String Int -> Map String Int -> LinkMe -> Int
resolveInitializer genv strings mmap (LMJust n) = n
resolveInitializer genv strings mmap (LMString str) = strings M.! str
resolveInitializer genv strings mmap (LMVariable name) = case M.lookup name genv of
  Nothing -> error ("global entity missing: " ++ name)
  Just (GlobalEntity _ size ty (link:_)) -> case ty of
    GloAtom -> resolveInitializer genv strings mmap link
    GloVec  -> mmap M.! name + 1
    GloFun  -> -1

-- 0. locate global entities in memory, generate the map
         --locateEntities
-- I. disallow duplicate named entities
         --dedupExtrns
-- II. require entities referenced by functions and each other to exist
         -- (reference check)
-- III. disallow cyclic references in initializers
         -- (cyclic initializer check)
-- IV. entity initialized to a string constant initialized with address of string
         -- already in two places
-- V. entity initialized with the name of another entity, traverse until value found
         --resolveInitializer
-- finally, given all the borates, produce the initial heap, entity map, string map, funmap
--       V . IV . III . II . I . 0

putGlobal :: Int -> GlobalEntity Int -> Heap -> Heap
putGlobal ptr (GlobalEntity _ size GloAtom [n]) h = Heap.poke ptr n h
putGlobal ptr (GlobalEntity _ size GloVec ns) h   = Heap.memcpy ptr ((ptr+1) : ns) h
putGlobal ptr (GlobalEntity _ size GloFun [-1]) h = Heap.poke ptr (-1) h

compileStringPlacement :: Int -> [String] -> (Int, Map String Int, Heap)
compileStringPlacement base strs = foldl visit (base, M.empty, Heap.empty) strs where
  visit (p,d,m) str = (p+size, d',m') where
    packed = packString (terminate str)
    size = length packed
    d' = M.insert str p d
    m' = Heap.memcpy p packed m

borateRawExtrns :: Borate LinkMe -> [GlobalEntity LinkMe]
borateRawExtrns (Borate as vs fs) = go where
  go = map atomNames as ++ map vectorNames vs ++ map funNames fs
  atomNames (name, link)          = GlobalEntity name 1 GloAtom [link]
  vectorNames (name, size, links) = GlobalEntity name (size+2) GloVec links
  funNames fun                    = GlobalEntity (funName fun) (funSize fun) GloFun [minus1]
  minus1 = LMJust (-1)
  
dedupExtrns :: [GlobalEntity LinkMe] -> Either String (Map String (GlobalEntity LinkMe))
dedupExtrns pile = go M.empty pile where
  go table [] = Right table
  go table (ent@(GlobalEntity name size _ links) : more) =
    case M.insertLookupWithKey (\_ x _ -> x) name ent table of
      (Nothing, table') -> go table' more
      (Just _, _) -> Left ("duplicate external name " ++ name)


