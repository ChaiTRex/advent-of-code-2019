import Control.Monad      (forM, replicateM, when)
import Data.Array.IO      (IOArray)
import Data.Array.MArray  (getElems, newListArray, readArray, writeArray)
import Data.List          (permutations)
import System.IO          (IOMode(ReadMode), hClose, hGetContents, openFile)
import System.IO.Unsafe   (unsafePerformIO)

runProgram :: IOArray Integer Integer -> [Integer] -> IO [Integer]
runProgram mem is = go 0 mem is []
  where
    go :: Integer -> IOArray Integer Integer -> [Integer] -> [Integer] -> IO [Integer]
    go instrPtr mem is os = do
      instr <- readArray mem instrPtr
      case instr of
           1    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                      
                      writeArray mem k (a + b)
                      go (instrPtr + 4) mem is os
           2    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                      
                      writeArray mem k (a * b)
                      go (instrPtr + 4) mem is os
           3    -> do
                      i <- readArray mem (instrPtr + 1)
                      
                      let (x:xs) = is
                      
                      writeArray mem i x
                      go (instrPtr + 2) mem xs os
           4    -> do
                      i <- readArray mem (instrPtr + 1)
                      
                      a <- readArray mem i
                      
                      go (instrPtr + 2) mem is (os ++ [a])
           5    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                      
                      if a /= 0 then go b              mem is os
                                else go (instrPtr + 3) mem is os
           105  -> do
                      j <- readArray mem (instrPtr + 2)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      if a /= 0 then go b              mem is os
                                else go (instrPtr + 3) mem is os
           1005 -> do
                      i <- readArray mem (instrPtr + 1)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      if a /= 0 then go b              mem is os
                                else go (instrPtr + 3) mem is os
           1105 -> do
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      if a /= 0 then go b              mem is os
                                else go (instrPtr + 3) mem is os
           6    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                     
                      if a == 0 then go b              mem is os
                                else go (instrPtr + 3) mem is os
           106  -> do
                      j <- readArray mem (instrPtr + 2)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      if a == 0 then go b              mem is os
                                else go (instrPtr + 3) mem is os
           1006 -> do
                      i <- readArray mem (instrPtr + 1)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      if a == 0 then go b              mem is os
                                else go (instrPtr + 3) mem is os
           1106 -> do
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      if a == 0 then go b              mem is os
                                else go (instrPtr + 3) mem is os
           7    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                      
                      if a < b then writeArray mem k 1
                               else writeArray mem k 0
                      go (instrPtr + 4) mem is os
           107  -> do
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      if a < b then writeArray mem k 1
                               else writeArray mem k 0
                      go (instrPtr + 4) mem is os
           1007 -> do
                      i <- readArray mem (instrPtr + 1)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      if a < b then writeArray mem k 1
                               else writeArray mem k 0
                      go (instrPtr + 4) mem is os
           1107 -> do
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      if a < b then writeArray mem k 1
                               else writeArray mem k 0
                      go (instrPtr + 4) mem is os
           8    -> do
                      i <- readArray mem (instrPtr + 1)
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem j
                      
                      if a == b then writeArray mem k 1
                                else writeArray mem k 0
                      go (instrPtr + 4) mem is os
           108  -> do
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      if a == b then writeArray mem k 1
                                else writeArray mem k 0
                      go (instrPtr + 4) mem is os
           1008 -> do
                      i <- readArray mem (instrPtr + 1)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      if a == b then writeArray mem k 1
                                else writeArray mem k 0
                      go (instrPtr + 4) mem is os
           1108 -> do
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      if a == b then writeArray mem k 1
                                else writeArray mem k 0
                      go (instrPtr + 4) mem is os
           99   -> return os
           101  -> do
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      writeArray mem k (a + b)
                      go (instrPtr + 4) mem is os
           102  -> do
                      j <- readArray mem (instrPtr + 2)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem j
                      
                      writeArray mem k (a * b)
                      go (instrPtr + 4) mem is os
           104  -> do
                      a <- readArray mem (instrPtr + 1)
                      
                      print a
                      go (instrPtr + 2) mem is os
           1001 -> do
                      i <- readArray mem (instrPtr + 1)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      writeArray mem k (a + b)
                      go (instrPtr + 4) mem is os
           1002 -> do
                      i <- readArray mem (instrPtr + 1)
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem i
                      b <- readArray mem (instrPtr + 2)
                      
                      writeArray mem k (a * b)
                      go (instrPtr + 4) mem is os
           1101 -> do
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      writeArray mem k (a + b)
                      go (instrPtr + 4) mem is os
           1102 -> do
                      k <- readArray mem (instrPtr + 3)
                      
                      a <- readArray mem (instrPtr + 1)
                      b <- readArray mem (instrPtr + 2)
                      
                      writeArray mem k (a * b)
                      go (instrPtr + 4) mem is os
           code -> fail $ "invalid opcode " ++ show code ++ " at IP=" ++ show instrPtr

main :: IO ()
main = do
  ps <- fmap (map read . lines . map (\ x -> if x == ',' then '\n' else x)) $ getContents :: IO [Integer]
  ts <- mapM (runAmps ps) . permutations $ [0 .. 4]
  print . maximum $ ts
  ts <- mapM (runAmps ps) . permutations $ [5 .. 9]
  print . maximum $ ts

runAmps :: [Integer] -> [Integer] -> IO (Integer, Integer)
runAmps ps [t1, t2, t3, t4, t5] = do
  mem1 <- newListArray (0, fromIntegral (length ps - 1)) ps
  mem2 <- newListArray (0, fromIntegral (length ps - 1)) ps
  mem3 <- newListArray (0, fromIntegral (length ps - 1)) ps
  mem4 <- newListArray (0, fromIntegral (length ps - 1)) ps
  mem5 <- newListArray (0, fromIntegral (length ps - 1)) ps
  let o1s = unsafePerformIO $ runProgram mem1 (t1:0:o5s) 
      o2s = unsafePerformIO $ runProgram mem2 (t2:o1s)
      o3s = unsafePerformIO $ runProgram mem3 (t3:o2s) 
      o4s = unsafePerformIO $ runProgram mem4 (t4:o3s) 
      o5s = unsafePerformIO $ runProgram mem5 (t5:o4s) 
  
  return (last o5s, 10000*t1 + 1000*t2 + 100*t3 + 10*t4 + t5)
