module Main (main) where

-- subset of MIPS instructions, (add, mov, lw, sw)

data Instruction = 
    ADD Reg Reg |
    MOV Reg Reg |
    LW Reg Int |
    SW Reg Int |
    NOP
  deriving (Show, Eq)
  
data Reg = 
    R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
  deriving (Show, Eq, Enum)
  
sampleInstructions :: [Instruction]
sampleInstructions = 
  [ ADD R1 R2
  , MOV R2 R4
  , LW R3 4
  , SW R4 4
  , NOP
  ]
  
-- A simple function to execute the instructions
executeInstruction :: CPU -> Instruction -> IO CPU
executeInstruction cpu (ADD r1 r2) = do 
  putStrLn ("Adding " ++ show r1 ++ " and " ++ show r2)
  return (doTick cpu (ADD r1 r2))
executeInstruction cpu (MOV r1 r2) = do
  putStrLn ("Moving " ++ show r1 ++ " to " ++ show r2)
  return (doTick cpu (MOV r1 r2))
executeInstruction cpu (LW r i) = do
  putStrLn ("Loading word from memory address " ++ show i ++ " to register " ++ show r)
  return (doTick cpu (LW r i))
executeInstruction cpu (SW r i) = do
  putStrLn ("Storing word from register " ++ show r ++ " to memory address " ++ show i)
  return (doTick cpu (SW r i))
executeInstruction cpu NOP = do
  putStrLn "No operation"
  return (doTick cpu NOP)

-- A simple function to execute a list of instructions
executeInstructions :: CPU -> [Instruction] -> IO CPU
executeInstructions cpu [] = return cpu
executeInstructions cpu (instr:instrs) = do
  newCpu <- executeInstruction cpu instr
  executeInstructions newCpu instrs
  
data CPU = 
    CPU { reg :: [Int], 
          pc :: Int, 
          mem :: [Int] }
  deriving (Show) 
  
-- Initialize the CPU with some registers and memory
initCPU :: CPU
initCPU = CPU { reg = [2, 3, 4, 5, 6, 7, 8, 9], pc = 0, mem = replicate 64 0 }

-- map registers to their values
mapRegToValue :: CPU -> Reg -> Int
mapRegToValue cpu R0 = (reg cpu) !! 0
mapRegToValue cpu R1 = (reg cpu) !! 1
mapRegToValue cpu R2 = (reg cpu) !! 2
mapRegToValue cpu R3 = (reg cpu) !! 3
mapRegToValue cpu R4 = (reg cpu) !! 4
mapRegToValue cpu R5 = (reg cpu) !! 5
mapRegToValue cpu R6 = (reg cpu) !! 6
mapRegToValue cpu R7 = (reg cpu) !! 7

-- Update the CPU state based on the instruction
doTick :: CPU -> Instruction -> CPU
doTick cpu (ADD r1 r2) = 
  let val1 = mapRegToValue cpu r1
      val2 = mapRegToValue cpu r2
      newReg = take (fromEnum r1) (reg cpu) ++ [val1 + val2] ++ drop (fromEnum r1 + 1) (reg cpu)
  in cpu { reg = newReg, pc = pc cpu + 1 }
doTick cpu (MOV r1 r2) =
  let val = mapRegToValue cpu r1
      newReg = take (fromEnum r2) (reg cpu) ++ [val] ++ drop (fromEnum r2 + 1) (reg cpu)
  in cpu { reg = newReg, pc = pc cpu + 1 }
doTick cpu (LW r i) =
  let val = mem cpu !! i
      newReg = take (fromEnum r) (reg cpu) ++ [val] ++ drop (fromEnum r + 1) (reg cpu)
  in cpu { reg = newReg, pc = pc cpu + 1 }
doTick cpu (SW r i) =
  let val = mapRegToValue cpu r
      newMem = take i (mem cpu) ++ [val] ++ drop (i + 1) (mem cpu)
  in cpu { mem = newMem, pc = pc cpu + 1 }
doTick cpu NOP = cpu { pc = pc cpu + 1 }
 
-- Main function to run the program
main :: IO ()
main = do
  let cpu = initCPU
  putStrLn "Initial CPU State:"
  print cpu
  putStrLn "\nExecuting Instructions:"
  let finalCpu = executeInstructions cpu sampleInstructions
  putStrLn "\nFinal CPU State:"
  finalCpuState <- finalCpu
  print finalCpuState
  return ()
