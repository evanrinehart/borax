{-# LANGUAGE DeriveGeneric #-}
module Main where

import Syntax
import Parser

main = do
  l <- getContents
  let result = Parser.parse "stdin" l
  case result of
    Right p  -> print p
    Left err -> Parser.printError err

