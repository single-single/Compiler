{-
Compilers (COMP3012) Coursework, 2021
  Shiqi XIN - 20125731
  (based on: Venanzio Capretta
             Nicolai Kraus)

Main executable.
-}

module Main where

import MiniTriangleParser
import TAMGenerator
import TAMExecutor
import TAM

import System.Environment
import Data.Char

-- The two executable file formats
data FileType = MT | TAM
  deriving (Eq,Show)

-- If input file has extension .exp, compile the expression to TAM code
-- If input file has extension .tam, execute tam code
main :: IO ()
main = do
  args <- getArgs
  let (fileName, extension) = fileNE args

  case extension of
    TAM -> do
      src <- readFile (fileName ++ ".tam")
      stk <- execTAM (parseTAM src)
      putStrLn ("Final stack: " ++ (show stk))
    MT -> do
      src <- readFile (fileName ++ ".mt")
      writeFile (fileName ++ ".tam") (compWrite src)
      >> putStrLn ("Compiled to TAM file: " ++ fileName ++ ".tam")

-- Finding the base name of a file name
baseName :: String -> String
baseName = takeWhile (/='.')

-- Finding the extension of a file name
fileExt :: String -> String
fileExt = dropWhile (/='.')

-- Converting a string of extension to FileType type
extType :: String -> Maybe FileType
extType ".mt" = Just MT
extType ".tam" = Just TAM
extType _ = Nothing

-- Extracting the filename and extension from an argument
parseFileName :: String -> Maybe (String, FileType)
parseFileName arg = do
  if isAlpha (head arg)
    then let name = baseName arg
             ext  = extType (fileExt arg)
         in case ext of
              Just t -> Just (name, t)
              Nothing -> Nothing
    else Nothing

-- Converting a list of Maybe a to one of a
unJust :: [Maybe a] -> [a]
unJust [] = []
unJust (Nothing:as) = unJust as
unJust (Just a:as) = a : unJust as

-- We assume one of the arguments is a file name
fileNE :: [String] -> (String, FileType)
fileNE = head . unJust . (map parseFileName)

-- Compiling MiniTriangle code into TAM code
comp :: String -> [TAMInst]
comp = progCode . progParse

-- Compiling and writing the result as a string
compWrite :: String -> String
compWrite = writeTAM . comp
