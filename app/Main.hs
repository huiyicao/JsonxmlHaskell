{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveGeneric #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson 
import GHC.Generics 
import JSON
import XmlRx.Types
import XmlRx.XmlRender 
import System.Environment
import System.FilePath.Posix
import qualified Data.ByteString.Lazy       as B
import qualified Text.XML                   as X

main = doLoop

doLoop = do
  putStrLn "Enter a command 'c' to convert or 'q' to quit:"
  command <- getLine
  case command of
    'q':_ -> return ()
    'c':_ -> do putStrLn "Enter JSON file path:"
                doConvertJson
                doLoop
    _        -> doLoop

-- Must input full path
-- -- XML file will be saved at the same directory with the same file name
doConvertJson :: IO ()
doConvertJson = do
  jsonFile <- getLine
  d <- (eitherDecode <$> (B.readFile jsonFile) :: IO (Either String Recipe))
  case d of
    Left err -> putStrLn err
    Right ps -> X.writeFile (X.def { X.rsPretty = False }) (replaceExtension jsonFile ".xml") (recipeDocument ps)
  
