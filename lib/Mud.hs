{-# LANGUAGE LambdaCase #-}
module Mud where

import System.Directory
import System.FilePath
import Data.List

import Syntax
import Parser
import Compile
import Eval

import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as M
-- a MUD interface to the compiler system.

-- room, the room you're in contains images of any b source files in the directory
-- or in the subdir.

-- if you compile b file you get a bo3 file. This is an incomplete program, it will
-- contain dangling references, which you can list. It also lists functions and
-- external objects defined within.

-- if you have a bunch of bo3 files which link together correctly, they can be
-- link to get a borax file. This is a full program ready to run.

-- bo3 and borax can be dumped to actual files.


-- return the filenames of all .b files
look :: FilePath -> IO [FilePath]
look path = map (path </>) . filter (".b" `isSuffixOf`) <$> listDirectory path


quickc :: FilePath -> IO (FunctionDef, CodeGraph Expr)
quickc path = do
  src <- readFile path
  let Right (Program [DefF fdef@(FunctionDef _ _ _ body)]) = parse path src
  let Right (_, g) = compileFunction body
  return (fdef,g)


-- load and run a self-contained function (main) in a file by itself
oneShot :: FilePath -> IO ()
oneShot path = do
  src <- readFile path
  case parse path src of
    Left err -> printError err
    Right (Program [DefF fdef]) -> do
      let FunctionDef _ funcname params body = fdef
      putStrLn ("funcname: " ++ funcname)
      putStrLn ("params: " ++ show params)
      case compileFunction body of
        Left (line,msg) -> print (line,msg)
        Right (start,gr) -> do
          putStrLn "body:"
          putStr (showGraph gr)
          putStrLn ("start node: " ++ show start)
          let layout = analyzeFrameLayout fdef
          let autoSize = frameAutoSize layout
          let extlist = extrnVariablesInBody body
          let localMap = frameNamesMap layout
          let numlabels = length (labelsUsedInBody body)
          let vecs = frameVectorLocations layout
          print ("layout", layout)
          print ("local map", localMap)
          print ("labels", numlabels)
          putStrLn "\nstarting program..."
          let func = Func gr autoSize extlist localMap (1 + numlabels) vecs start
          runShot func

--frameNamesMap :: [FrameObj] -> Map String Int
--frameVectorLocations :: [FrameObj] -> [Int]
--frameAutoSize :: [FrameObj] -> Int



extrnVariablesInBody :: Statement -> [String]
extrnVariablesInBody = execWriter . go where
  go :: Statement -> Writer [String] ()
  go (AutoStatement _ _ stmt) = go stmt
  go (ExtrnStatement _ vars stmt) = do
    tell vars
    go stmt
  go (LabelStatement _ _ stmt) = go stmt
  go (CaseStatement _ _ stmt) = go stmt
  go (CompoundStatement _ stmts) = mapM_ go stmts
  go (ConditionalStatement _ _ stmt Nothing) = go stmt
  go (ConditionalStatement _ _ stmt1 (Just stmt2)) = do
    go stmt1
    go stmt2
  go (WhileStatement _ _ stmt) = go stmt
  go (SwitchStatement _ _ stmt) = go stmt
  go _ = return ()

labelsUsedInBody :: Statement -> [String]
labelsUsedInBody = execWriter . go where
  go :: Statement -> Writer [String] ()
  go (AutoStatement _ _ stmt) = go stmt
  go (ExtrnStatement _ _ stmt) = go stmt
  go (LabelStatement _ name stmt) = tell [name] >> go stmt
  go (CaseStatement _ _ stmt) = go stmt
  go (CompoundStatement _ stmts) = mapM_ go stmts
  go (ConditionalStatement _ _ stmt Nothing) = go stmt
  go (ConditionalStatement _ _ stmt1 (Just stmt2)) = do
    go stmt1
    go stmt2
  go (WhileStatement _ _ stmt) = go stmt
  go (SwitchStatement _ _ stmt) = go stmt
  go _ = return ()




