{-# LANGUAGE FlexibleInstances #-}

module Intcode where

import qualified Data.IntMap.Lazy as IntMap  (IntMap, delete, findWithDefault, fromList, insert, toList)

import Control.Arrow  (first)
import Data.Word      (Word)

-- Step type, a continuation at the beginning of each instruction, specifying any interactions with the environment

data Step = Normal Machine Step
          | Input  Machine (Integer -> Step)
          | Output Machine Integer Step
          | Halted Machine
          | Error  Machine String
  deriving (Show)

initialize :: Memory -> Step
initialize mem = let m = Machine mem 0 0
                 in  Normal m (step m)

step :: Machine -> Step
step m = let instr = decodeCurrentInstruction m
         in  case instr of
                  Left  msg    -> Error m msg
                  Right instr' -> exec m instr'
  where
    exec :: Machine -> Instruction -> Step
    exec m instr = let mem            = memory m
                       ip             = instructionPointer m
                       rb             = relativeBase m
                       read  (Ptr a)  = Right $ mem ! a
                       read  (Imm i)  | i < 0                                 = Left $ "Bad instruction: read from negative address " ++ show i ++ "."
                                      | i > toInteger (maxBound :: Word)      = Left $ "Bad instruction: read from too-large address " ++ show i ++ "."
                                      | otherwise                             = Right $ mem ! fromInteger i
                       read  (Rel i)  | rb + i < 0                            = Left $ "Bad address: read from relative base of " ++ show rb ++ " + " ++ show i ++ " is negative address."
                                      | rb + i > toInteger (maxBound :: Word) = Left $ "Bad address: read from relative base of " ++ show rb ++ " + " ++ show i ++ " is too-large address."
                                      | otherwise                             = Right $ mem ! fromInteger (rb + i)
                       writer :: Value -> Either String (Integer -> Memory)
                       writer (Ptr a) = Right $ \ x -> set a x mem
                       writer (Imm _) = Left "Bad instruction: cannot write to an immediate value."
                       writer (Rel i) | rb + i < 0                            = Left $ "Bad address: write to relative base of " ++ show rb ++ " + " ++ show i ++ " is negative address."
                                      | rb + i > toInteger (maxBound :: Word) = Left $ "Bad address: write to relative base of " ++ show rb ++ " + " ++ show i ++ " is too-large address."
                                      | otherwise                             = Right $ \ x -> set (fromInteger (rb + i)) x mem
                       thisStep       = case instr of
                                             Add v1 v2 v3 -> do
                                                                val <- (+) <$> read v1 <*> read v2
                                                                f   <- writer v3
                                                                let m' = m { memory = f val, instructionPointer = ip + 4 }
                                                                return $ Normal m' (step m')
                                             Mul v1 v2 v3 -> do
                                                                val <- (*) <$> read v1 <*> read v2
                                                                f   <- writer v3
                                                                let m' = m { memory = f val, instructionPointer = ip + 4 }
                                                                return $ Normal m' (step m')
                                             In  v1       -> do
                                                                f <- writer v1
                                                                return $ Input m (\ val -> let m' = m { memory = f val, instructionPointer = ip + 2 }
                                                                                           in  step m'
                                                                                 )
                                             Out v1       -> do
                                                                val <- read v1
                                                                let m' = m { instructionPointer = ip + 2 }
                                                                return $ Output m' val (step m')
                                             JNZ v1 v2    -> do
                                                                val <- read v1
                                                                jmp <- read v2
                                                                if jmp < 0
                                                                   then Left $ "Bad address: jump target " ++ show jmp ++ " is negative address."
                                                                   else if jmp > toInteger (maxBound :: Word)
                                                                           then Left $ "Bad address: jump target " ++ show jmp ++ "is too-large address."
                                                                           else let m' = m { instructionPointer = if val == 0 then ip + 3 else fromInteger jmp }
                                                                                in  return $ Normal m' (step m')
                                             JZ  v1 v2    -> do
                                                                val <- read v1
                                                                jmp <- read v2
                                                                if jmp < 0
                                                                   then Left $ "Bad address: jump target " ++ show jmp ++ " is negative address."
                                                                   else if jmp > toInteger (maxBound :: Word)
                                                                           then Left $ "Bad address: jump target " ++ show jmp ++ "is too-large address."
                                                                           else let m' = m { instructionPointer = if val /= 0 then ip + 3 else fromInteger jmp }
                                                                                in  return $ Normal m' (step m')
                                             LTh v1 v2 v3 -> do
                                                                val <- (\ x y -> if x < y then 1 else 0) <$> read v1 <*> read v2
                                                                f   <- writer v3
                                                                let m' = m { memory = f val, instructionPointer = ip + 4 }
                                                                return $ Normal m' (step m')
                                             Equ v1 v2 v3 -> do
                                                                val <- (\ x y -> if x == y then 1 else 0) <$> read v1 <*> read v2
                                                                f   <- writer v3
                                                                let m' = m { memory = f val, instructionPointer = ip + 4 }
                                                                return $ Normal m' (step m')
                                             RUp v1       -> do
                                                                val <- read v1
                                                                let m' = m { relativeBase = rb + val, instructionPointer = ip + 2 }
                                                                return $ Normal m' (step m')
                                             Hlt          -> return $ Halted m
                   in  case thisStep of
                            Left  s -> Error m s
                            Right s -> s

instance Show (Integer -> Step) where
  showsPrec p m = showParen (p > 10) $ showString "<<Integer -> Step>>"

-- Machine type, the current state of an Intcode machine

data Machine = Machine { memory             :: Memory
                       , instructionPointer :: Word
                       , relativeBase       :: Integer
                       }
  deriving (Show, Eq, Ord)

data Instruction = Add Value Value Value
                 | Mul Value Value Value
                 | In  Value
                 | Out Value
                 | JNZ Value Value
                 | JZ  Value Value
                 | LTh Value Value Value
                 | Equ Value Value Value
                 | RUp Value
                 | Hlt
  deriving (Show, Eq, Ord)

data Value = Ptr Word
           | Imm Integer
           | Rel Integer
  deriving (Show, Eq, Ord)

decodeCurrentInstruction :: Machine -> Either String Instruction
decodeCurrentInstruction m = decodeInstruction (memory m) (instructionPointer m)

decodeInstruction :: Memory -> Word -> Either String Instruction
decodeInstruction mem ip = let opcode        = mem ! ip
                               (modes, base) = quotRem opcode 100
                               f             = case base of
                                                    1  -> op3 True  Add
                                                    2  -> op3 True  Mul
                                                    3  -> op1 True  In
                                                    4  -> op1 False Out
                                                    5  -> op2 False JNZ
                                                    6  -> op2 False JZ
                                                    7  -> op3 True  LTh
                                                    8  -> op3 True  Equ
                                                    9  -> op1 False RUp
                                                    99 -> op0       Hlt
                                                    _  -> \ _ _ _ -> Left $ "Bad instruction: opcode base " ++ show base ++ " has no meaning."
                           in  f modes mem ip
  where
    op0 :: Instruction -> Integer -> Memory -> Word -> Either String Instruction
    op0 instr 0     _ _ = Right instr
    op0 _     modes _ _ = Left $ "Bad instruction: addressing mode digits " ++ show modes ++ " left over after using all needed."
    
    op1 :: Bool -> (Value -> Instruction) -> Integer -> Memory -> Word -> Either String Instruction
    op1 writes instr modes mem addr = if addr == maxBound
                                         then Left "Memory address overflow: instruction goes past end of memory."
                                         else let (modes', mode) = quotRem modes 10
                                                  addr'          = addr + 1
                                                  val            = mem ! addr'
                                              in  case mode of
                                                       0 -> if val < 0
                                                               then Left "Bad instruction: pointer to negative address."
                                                               else if val > toInteger (maxBound :: Word)
                                                                       then Left "Bad instruction: pointer goes past end of memory."
                                                                       else op0 (instr (Ptr (fromInteger val))) modes' mem addr'
                                                       1 -> if writes
                                                               then Left "Bad instruction: invalid mode digit 1 should be 0 or 2."
                                                               else op0 (instr (Imm val)) modes' mem addr'
                                                       2 -> op0 (instr (Rel val)) modes' mem addr'
                                                       _ -> if writes
                                                               then Left $ "Bad instruction: invalid mode digit " ++ show mode ++ " should be 0 or 2."
                                                               else Left $ "Bad instruction: invalid mode digit " ++ show mode ++ " should be 0, 1, or 2."
    
    op2 :: Bool -> (Value -> Value -> Instruction) -> Integer -> Memory -> Word -> Either String Instruction
    op2 writes instr modes mem addr = if addr == maxBound
                                         then Left "Memory address overflow: instruction goes past end of memory."
                                         else let (modes', mode) = quotRem modes 10
                                                  addr'          = addr + 1
                                                  val            = mem ! addr'
                                              in  case mode of
                                                       0 -> if val < 0
                                                               then Left "Bad instruction: pointer to negative address."
                                                               else if val > toInteger (maxBound :: Word)
                                                                       then Left "Bad instruction: pointer goes past end of memory."
                                                                       else op1 writes (instr (Ptr (fromInteger val))) modes' mem addr'
                                                       1 -> op1 writes (instr (Imm val)) modes' mem addr'
                                                       2 -> op1 writes (instr (Rel val)) modes' mem addr'
                                                       _ -> Left $ "Bad instruction: invalid mode digit " ++ show mode ++ " should be 0, 1, or 2."
    
    op3 :: Bool -> (Value -> Value -> Value -> Instruction) -> Integer -> Memory -> Word -> Either String Instruction
    op3 writes instr modes mem addr = if addr == maxBound
                                         then Left "Memory address overflow: instruction goes past end of memory."
                                         else let (modes', mode) = quotRem modes 10
                                                  addr'          = addr + 1
                                                  val            = mem ! addr'
                                              in  case mode of
                                                       0 -> if val < 0
                                                               then Left "Bad instruction: pointer to negative address."
                                                               else if val > toInteger (maxBound :: Word)
                                                                       then Left "Bad instruction: pointer goes past end of memory."
                                                                       else op2 writes (instr (Ptr (fromInteger val))) modes' mem addr'
                                                       1 -> op2 writes (instr (Imm val)) modes' mem addr'
                                                       2 -> op2 writes (instr (Rel val)) modes' mem addr'
                                                       _ -> Left $ "Bad instruction: invalid mode digit " ++ show mode ++ " should be 0, 1, or 2."

-- Memory type, based on IntMap, with Word addresses

newtype Memory = Memory (IntMap.IntMap Integer)
 deriving (Eq)

instance Show Memory where
  showsPrec p m = showParen (p > 10) $ showString "fromList " . shows (toList m)

instance Ord Memory where
  compare m1 m2 = compare (toList m1) (toList m2)

fromList :: [(Word, Integer)] -> Memory
fromList = Memory . IntMap.fromList . map (first fromIntegral) . filter ((/= 0) . snd)

toList :: Memory -> [(Word, Integer)]
toList (Memory m) = map (first fromIntegral) . shiftNegatives . IntMap.toList $ m
  where
    shiftNegatives :: [(Int, Integer)] -> [(Int, Integer)]
    shiftNegatives xs = let ys = takeWhile ((< 0) . fst) xs
                            zs = dropWhile ((< 0) . fst) xs
                        in  zs ++ ys

set :: Word -> Integer -> Memory -> Memory
set i 0 (Memory m) = Memory . IntMap.delete (fromIntegral i)   $ m
set i x (Memory m) = Memory . IntMap.insert (fromIntegral i) x $ m

(!) :: Memory -> Word -> Integer
Memory m ! i = IntMap.findWithDefault 0 (fromIntegral i) m

