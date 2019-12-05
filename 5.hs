import Control.Monad      (forM, when)
import Data.Array.IO      (IOArray)
import Data.Array.MArray  (getElems, newListArray, readArray, writeArray)
import System.IO          (IOMode(ReadMode), hClose, hGetContents, openFile)

runProgram :: IOArray Integer Integer -> IO ()
runProgram = go 0
  where
    go :: Integer -> IOArray Integer Integer -> IO ()
    go instrPtr mem = do
      instr <- readArray mem instrPtr
      case instr of
           1    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                      
                      writeArray mem k (a + b)
                      go (instrPtr + 4) mem
           2    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                      
                      writeArray mem k (a * b)
                      go (instrPtr + 4) mem
           3    -> do
                      i <- readArray mem (instrPtr + 1)
                     
                      a <- fmap read getLine
                      
                      writeArray mem i a
                      go (instrPtr + 2) mem
           4    -> do
                      i <- readArray mem (instrPtr + 1)
                      
                      a <- readArray mem i
                      
                      print a
                      go (instrPtr + 2) mem
           5    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                      
                      if a /= 0 then go b              mem
                                else go (instrPtr + 3) mem
           105  -> do
                      j <- readArray mem (instrPtr + 2)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      if a /= 0 then go b              mem
                                else go (instrPtr + 3) mem
           1005 -> do
                      i <- readArray mem (instrPtr + 1)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      if a /= 0 then go b              mem
                                else go (instrPtr + 3) mem
           1105 -> do
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      if a /= 0 then go b              mem
                                else go (instrPtr + 3) mem
           6    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                     
                      if a == 0 then go b              mem
                                else go (instrPtr + 3) mem
           106  -> do
                      j <- readArray mem (instrPtr + 2)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      if a == 0 then go b              mem
                                else go (instrPtr + 3) mem
           1006 -> do
                      i <- readArray mem (instrPtr + 1)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      if a == 0 then go b              mem
                                else go (instrPtr + 3) mem
           1106 -> do
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      if a == 0 then go b              mem
                                else go (instrPtr + 3) mem
           7    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                      
                      if a < b then writeArray mem k 1
                               else writeArray mem k 0
                      go (instrPtr + 4) mem
           107  -> do
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      if a < b then writeArray mem k 1
                               else writeArray mem k 0
                      go (instrPtr + 4) mem
           1007 -> do
                      i <- readArray mem (instrPtr + 1)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      if a < b then writeArray mem k 1
                               else writeArray mem k 0
                      go (instrPtr + 4) mem
           1107 -> do
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      if a < b then writeArray mem k 1
                               else writeArray mem k 0
                      go (instrPtr + 4) mem
           8    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                      
                      if a == b then writeArray mem k 1
                                else writeArray mem k 0
                      go (instrPtr + 4) mem
           108  -> do
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      if a == b then writeArray mem k 1
                                else writeArray mem k 0
                      go (instrPtr + 4) mem
           1008 -> do
                      i <- readArray mem (instrPtr + 1)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      if a == b then writeArray mem k 1
                                else writeArray mem k 0
                      go (instrPtr + 4) mem
           1108 -> do
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      if a == b then writeArray mem k 1
                                else writeArray mem k 0
                      go (instrPtr + 4) mem
           99   -> return ()
           101  -> do
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      writeArray mem k (a + b)
                      go (instrPtr + 4) mem
           102  -> do
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      writeArray mem k (a * b)
                      go (instrPtr + 4) mem
           104  -> do
                      a <- readArray mem (instrPtr + 1)
                      
                      print a
                      go (instrPtr + 2) mem
           1001 -> do
                      i <- readArray mem (instrPtr + 1)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      writeArray mem k (a + b)
                      go (instrPtr + 4) mem
           1002 -> do
                      i <- readArray mem (instrPtr + 1)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      writeArray mem k (a * b)
                      go (instrPtr + 4) mem
           1101 -> do
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      writeArray mem k (a + b)
                      go (instrPtr + 4) mem
           1102 -> do
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      writeArray mem k (a * b)
                      go (instrPtr + 4) mem
           code -> fail $ "invalid opcode " ++ show code ++ " at IP=" ++ show instrPtr

main :: IO ()
main = do
  pgmFile <- openFile "5.txt" ReadMode
  xs <- fmap (map read . lines . map (\ x -> if x == ',' then '\n' else x)) $ hGetContents pgmFile
  mem <- newListArray (0, fromIntegral (length xs - 1)) xs
  runProgram mem 
