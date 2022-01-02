module Main where

import qualified Image as Img
import qualified QuadTreeSim as SimQuad
import qualified Simulator as SimList
import System.IO

main :: IO ()
main = do
  putStrLn ("Welcome to ICANSQUAD!!\n" ++ "Which Application would you like to run?")
  instrs
  mainLoop

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

mainLoop :: IO ()
mainLoop = loop
  where
    loop = do
      input <- prompt "Main > "
      case input of
        "1" -> do
          Img.main
          loop
        "2" -> do
          SimQuad.main
          loop
        "3" -> do
          SimList.main
          loop
        ":q" -> return ()
        ":h" -> do
          instrs
          loop
        _ -> loop

instrs :: IO ()
instrs = do
  putStrLn "Your options are: "
  putStrLn "(1) Fun with Images"
  putStrLn "(2) N-Body Simulation QuadTree"
  putStrLn "(3) N-Body Simulation List"
  putStrLn "(:q) To exit"
  putStrLn "(:h) To print this message"