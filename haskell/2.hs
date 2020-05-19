import Control.Monad      (forM, when)
import Data.Array.IO      (IOArray)
import Data.Array.MArray  (getElems, newListArray, readArray, writeArray)

runProgram :: IOArray Integer Integer -> IO Integer
runProgram = go 0
  where
    go :: Integer -> IOArray Integer Integer -> IO Integer
    go instrPtr mem = do
      instr <- readArray mem instrPtr
      case instr of
           1  -> do
                    i <- readArray mem (instrPtr + 1)
                    j <- readArray mem (instrPtr + 2)
                    k <- readArray mem (instrPtr + 3)
                    
                    a <- readArray mem i
                    b <- readArray mem j
                    
                    writeArray mem k (a + b)
                    go (instrPtr + 4) mem
           2  -> do
                    i <- readArray mem (instrPtr + 1)
                    j <- readArray mem (instrPtr + 2)
                    k <- readArray mem (instrPtr + 3)
                    
                    a <- readArray mem i
                    b <- readArray mem j
                    
                    writeArray mem k (a * b)
                    go (instrPtr + 4) mem
           99 -> readArray mem 0
           _  -> fail "invalid opcode" 

tryWith :: Integer -> Integer -> [Integer] -> IO (Maybe Integer)
tryWith a b xs = do
  let max = fromIntegral (length xs - 1)
  if      a > max then return Nothing
  else if b > max then return Nothing
  else                 do
                          mem <- newListArray (0, max) xs
                          writeArray mem 1 a
                          writeArray mem 2 b
                          result <- runProgram mem
                          return (Just result)

main :: IO ()
main = do
  xs <- fmap (map read . lines . map (\ x -> if x == ',' then '\n' else x)) getContents
  Just result <- tryWith 12 2 xs
  print result
  
  let max = fromIntegral (length xs - 1)
  results <- forM [0 .. max] $ \a -> do
               forM [0 .. max] $ \b -> do
                 result <- tryWith a b xs
                 return (a, b, result)
  print . map (\ (a, b, _) -> 100*a + b) . filter (\ (a, b, result) -> result == Just 19690720) . concat $ results
