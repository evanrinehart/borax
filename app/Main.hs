module Main where

import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Data.Char
import Data.Time.Clock.POSIX
import Data.Functor

import Syntax
import Parser
import Compile
import Link
import Eval
import System

import Text.Pretty.Simple

main = do

  hSetBuffering stdout NoBuffering

  paths <- getPathsFromCmdLine
  borates <- forM paths $ \path -> do
    boron <- parseFile path
    case compile boron of
      Left msg  -> putStrLn msg >> exitFailure
      Right bor -> return bor
  case link borates of
    Left msg  -> putStrLn msg >> exitFailure
    Right borax -> do
      let machine = fromBorax actualServices borax
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

actualServices :: ServiceCalls IO
actualServices = dummySystem
  { service_putchar = ($> 0) . putChar . chr
  , service_getchar = inputService
  , service_time    = timeService
  , service_exit    = exitSuccess
  }

-- get the current system time and return it in two words
timeService :: IO (Int,Int)
timeService = do
  t <- getPOSIXTime
  let ticks = floor (t * 60) :: Integer
  let (msw,lsw) = ticks `divMod` (2 ^ 36)
  return (fromIntegral msw, fromIntegral lsw)

-- get the next character from standard input. Return '*e' if end of file.
inputService :: IO Int
inputService = do
  eof <- isEOF
  if eof
    then return 4
    else ord <$> getChar
