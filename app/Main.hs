{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.IO
import System.Environment
import System.Exit
import Control.Monad

import Syntax
import Parser
import Compile
import Link
import Eval

import Text.Pretty.Simple

main = do
  paths <- getPathsFromCmdLine
  --pPrint paths
  borates <- forM paths $ \path -> do
    boron <- parseFile path
    case compile boron of
      Left msg  -> putStrLn msg >> exitFailure
      Right bor -> return bor
  --pPrint borates
  case link borates of
    Left msg  -> putStrLn msg >> exitFailure
    Right borax -> do
      --pPrint borax
      let machine = fromBorax borax
      result <- bootUp machine
      case result of
        Left msg -> putStrLn ("failure: " ++ msg)
        Right r  -> putStrLn ("success: " ++ show r)

getPathsFromCmdLine :: IO [String]
getPathsFromCmdLine = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "usage: list the source files to compile and run"
      exitSuccess
    paths -> return paths
