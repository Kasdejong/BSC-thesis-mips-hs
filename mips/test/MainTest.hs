module MainTest where

import Test.Hspec
import Main

-- Helper to get a register value
getReg :: CPU -> RegID -> Int
getReg cpu r = case mapRegIDToValue cpu r of
  Val x -> x
  Pending _ _ -> error "Register still pending"

-- Helper to check PC
getPC :: CPU -> Int
getPC = pc

-- Helper to get memory value
getMem :: CPU -> Int -> Int
getMem cpu addr = mem cpu !! addr

-- Custom CPU initializer for tests
initTestCPU :: [RegValue] -> [Int] -> [Instruction] -> CPU
initTestCPU regs mem prog = CPU
  { reg = regs
  , stations =
      [ ResStation { rsid = RS_ADD1, op = OP_ADD, vj = 0, vk = 0, qj = Nothing, qk = Nothing, a = 0, busy = False, robTagRS = Nothing }
      , ResStation { rsid = RS_ADD2, op = OP_ADD, vj = 0, vk = 0, qj = Nothing, qk = Nothing, a = 0, busy = False, robTagRS = Nothing }
      , ResStation { rsid = RS_LS1, op = OP_LW, vj = 0, vk = 0, qj = Nothing, qk = Nothing, a = 0, busy = False, robTagRS = Nothing }
      ]
  , rob = []
  , pc = 0
  , mem = mem
  , robOffset = 0
  , program = prog
  }

-- Initial register values for tests 1-3
testRegs :: [RegValue]
testRegs = 
  [ Val 0   -- R0
  , Val 1   -- R1
  , Val 3   -- R2
  , Val 2   -- R3
  , Val 3   -- R4
  , Val 1   -- R5
  , Val 3   -- R6
  , Val (-1) -- R7
  ]

-- Initial register values for tests 4-5 (different R2 and R3)
test45Regs :: [RegValue]
test45Regs = 
  [ Val 0   -- R0
  , Val 1   -- R1
  , Val 1   -- R2 (changed for tests 4-5)
  , Val 2   -- R3 (changed for tests 4-5)
  , Val 3   -- R4
  , Val 1   -- R5
  , Val 3   -- R6
  , Val (-1) -- R7
  ]

-- Memory initialized to 0
testMem :: [Int]
testMem = replicate 64 0

-- Helper to run a program with custom state
runProgramWithState :: [RegValue] -> [Int] -> [Instruction] -> IO CPU
runProgramWithState regs mem prog = executeInstructions (initTestCPU regs mem prog)

-- Test Program 1: Simple BEQ taken
-- BEQ jumps over the next instruction (offset 2)
test1Prog :: [Instruction]
test1Prog =
  [ INSTR_ADD R1 R2 R0    -- R1 = 3 + 0 = 3
  , INSTR_BEQ R1 R2 2      -- Taken: skip next instruction
  , INSTR_ADD R3 R3 R4     -- Should be skipped (R3 would be 2+3=5)
  , INSTR_NOP
  ]

-- Test Program 2: BNE not taken
-- BNE does not jump (offset 2)
test2Prog :: [Instruction]
test2Prog =
  [ INSTR_ADD R1 R2 R0    -- R1 = 3 + 0 = 3
  , INSTR_BNE R1 R2 2      -- Not taken (3==3), do not skip
  , INSTR_ADD R3 R3 R4     -- Executed: R3 = 2+3=5
  , INSTR_NOP
  ]

-- Test Program 3: Loop with counter
-- BNE jumps back to the second ADD (offset -1)
test3Prog :: [Instruction]
test3Prog =
  [ INSTR_ADD R5 R6 R0    -- R5 = 3 (R6=3)
  , INSTR_ADD R5 R5 R7     -- R5 = 3 + (-1) = 2
  , INSTR_BNE R5 R0 (-1)   -- Loop back to first ADD if R5 != 0
  , INSTR_NOP
  ]

-- Test Program 4: Data hazard with branch
-- BEQ jumps over the next instruction (offset 2)
test4Prog :: [Instruction]
test4Prog =
  [ INSTR_ADD R1 R2 R3    -- R1 = 1 + 2 = 3
  , INSTR_BEQ R1 R4 2      -- Taken (3==3), skip next
  , INSTR_ADD R5 R5 R6     -- Should be skipped
  , INSTR_NOP
  ]

-- Test Program 5: Complex hazard scenario
-- BEQ jumps over 2 instructions (offset 3)
test5Prog :: [Instruction]
test5Prog =
  [ INSTR_ADD R1 R2 R3    -- R1 = 1+2=3
  , INSTR_LW R3 8 R0      -- Load from mem[8] (0)
  , INSTR_BEQ R1 R4 3     -- Taken (3==3), jump over next 2
  , INSTR_ADD R5 R1 R3    -- Skipped
  , INSTR_SW R1 12 R0     -- Skipped
  , INSTR_ADD R6 R1 R1    -- R6 = 3+3=6
  , INSTR_NOP
  ]
  
test6Prog :: [Instruction]
test6Prog =
  [
    INSTR_ADD R4 R4 R4 -- R4 = 6
  , INSTR_ADD R4 R4 R4 -- R4 = 12
  , INSTR_ADD R4 R4 R4 -- R4 = 24
  , INSTR_BEQ R1 R4 4 -- while R1 != 24
  , INSTR_ADD R1 R1 R2 -- add one to R1
  , INSTR_ADD R1 R1 R2 -- add one to R1
  , INSTR_BEQ R0 R0 (-3)
  , INSTR_NOP
  ]

mvMultiplication :: [Instruction]
mvMultiplication =
  [
    -- Load matrix and vector elements
    INSTR_LW R3 0 R0,     -- R3 = a11
    INSTR_LW R4 16 R0,    -- R4 = b1
    -- R1 = a11 * b1
    INSTR_MOV R1 R0,      -- R1 = 0
    INSTR_MOV R5 R4,      -- R5 = b1 (counter)
    INSTR_MOV R6 R0,      -- R6 = 0 (accumulator)
    -- Loop a11 * b1
    INSTR_BEQ R5 R0 4,
    INSTR_ADD R6 R6 R3,
    INSTR_ADD R5 R5 R7,
    INSTR_BEQ R0 R0 (-3),
    INSTR_MOV R1 R6,      -- R1 = a11 * b1 result

    INSTR_LW R3 4 R0,     -- R3 = a12
    INSTR_LW R4 20 R0,    -- R4 = b2
    INSTR_MOV R5 R4,
    INSTR_MOV R6 R0,
    -- Loop a12 * b2
    INSTR_BEQ R5 R0 4,
    INSTR_ADD R6 R6 R3,
    INSTR_ADD R5 R5 R7,
    INSTR_BEQ R0 R0 (-3),
    INSTR_ADD R1 R1 R6,   -- R1 += a12 * b2

    INSTR_LW R3 8 R0,     -- R3 = a21
    INSTR_LW R4 16 R0,    -- R4 = b1
    INSTR_MOV R5 R4,
    INSTR_MOV R6 R0,
    -- Loop a21 * b1
    INSTR_BEQ R5 R0 4,
    INSTR_ADD R6 R6 R3,
    INSTR_ADD R5 R5 R7,
    INSTR_BEQ R0 R0 (-3),
    INSTR_MOV R2 R6,

    INSTR_LW R3 12 R0,    -- R3 = a22
    INSTR_LW R4 20 R0,    -- R4 = b2
    INSTR_MOV R5 R4,
    INSTR_MOV R6 R0,
    -- Loop a22 * b2
    INSTR_BEQ R5 R0 4,
    INSTR_ADD R6 R6 R3,
    INSTR_ADD R5 R5 R7,
    INSTR_BEQ R0 R0 (-3),
    INSTR_ADD R2 R2 R6,

    -- Store result
    INSTR_SW R1 24 R0,
    INSTR_SW R2 28 R0,
    INSTR_NOP
  ]

mvMultiplicationMem :: [Int]
mvMultiplicationMem =
  [ 1, 0, 0, 0   -- 0:  a11
  , 2, 0, 0, 0   -- 4:  a12
  , 3, 0, 0, 0   -- 8:  a21
  , 4, 0, 0, 0   -- 12: a22
  , 3, 0, 0, 0   -- 16: b1
  , 4, 0, 0, 0   -- 20: b2
  , 0, 0, 0, 0   -- 24: result[0]
  , 0, 0, 0, 0   -- 28: result[1]
  ]
  
mvMultiplicationRegs :: [RegValue]
mvMultiplicationRegs =
  [ Val 0, Val 0, Val 0, Val 0, Val 0, Val 0, Val 0, Val 0 ]

hazardTestProg :: [Instruction]
hazardTestProg =
  [ -- Memory hazards
    INSTR_LW R1 0 R0    -- R1 = 5
  , INSTR_SW R1 4 R0    -- mem[4] = 5
  
  -- Data hazards
  , INSTR_LW R2 16 R0  -- R2 = 1
  , INSTR_ADD R3 R1 R2  -- R3 = 6
  , INSTR_ADD R4 R3 R1  -- R4 = 11
  , INSTR_ADD R1 R4 R0  -- R1 = 11
  
  -- Control hazards
  , INSTR_LW R5 20 R0  -- R5 = 5
  , INSTR_ADD R5 R5 R2  -- R5 = 6
  , INSTR_BEQ R3 R5 3   -- Taken, skip next 2
  , INSTR_ADD R6 R6 R2  -- Should be skipped
  , INSTR_ADD R7 R7 R2  -- Should be skipped
  , INSTR_LW R6 8 R0    -- R6 = 10
  
  -- Branch delay hazards
  , INSTR_BEQ R6 R0 2   -- Not taken (R6=10 != 0)
  , INSTR_ADD R7 R6 R2  -- R7 = 11
  , INSTR_SW R7 12 R0   -- mem[12] = 11
  , INSTR_NOP
  ]

hazardTestMem = 
  [ 5,0,0,0, 0,0,0,0, 10,0,0,0, 0,0,0,0
  , 1,0,0,0, 5,0,0,0
  ] ++ replicate 250 0

hazardTestRegs = 
  [ Val 0, Val 0, Val 0, Val 0, Val 0, Val 0, Val 0, Val 0 ]

main :: IO ()
main = hspec $ do
  describe "Test Program 1: Simple BEQ taken" $ do
    it "should skip instruction after branch, R3 remains 2" $ do
      cpu <- runProgramWithState testRegs testMem test1Prog
      getReg cpu R3 `shouldBe` 2
  
  describe "Test Program 2: BNE not taken" $ do
    it "should execute instruction after branch, R3 becomes 5" $ do
      cpu <- runProgramWithState testRegs testMem test2Prog
      getReg cpu R3 `shouldBe` 5
  
  describe "Test Program 3: Loop with counter" $ do
    it "should loop until R5 becomes 0" $ do
      cpu <- runProgramWithState testRegs testMem test3Prog
      getReg cpu R5 `shouldBe` 0
  
  describe "Test Program 4: Data hazard with branch" $ do
    it "should skip instruction after branch, R5 remains 1" $ do
      cpu <- runProgramWithState test45Regs testMem test4Prog
      getReg cpu R5 `shouldBe` 1
  
  describe "Test Program 5: Complex hazard scenario" $ do
    it "should execute branch target, R6=6 and mem[12]=0" $ do
      cpu <- runProgramWithState test45Regs testMem test5Prog
      getReg cpu R6 `shouldBe` 6
      getMem cpu 12 `shouldBe` 0
      
  describe "Test Program 6: Simple ADDs" $ do
    it "should compute R1 correctly after 3 ADDs" $ do
      cpu <- runProgramWithState test45Regs testMem test6Prog
      getReg cpu R1 `shouldBe` 24  -- R1 = 3 + 3 + 3
    
--  describe "Matrix vector multiplication" $ do
--    it "computes MV mult correctly" $ do
--      cpu <- runProgramWithState mvMultiplicationRegs mvMultiplicationMem mvMultiplication
--      getMem cpu 24 `shouldBe` 11  -- result[0]
--      getMem cpu 28 `shouldBe` 25  -- result[1]
  
  describe "Hazard Stress Test (8 registers)" $ do
    it "handles all hazard types correctly" $ do
      cpu <- runProgramWithState hazardTestRegs hazardTestMem hazardTestProg
      getReg cpu R1 `shouldBe` 11
      getReg cpu R7 `shouldBe` 11
      getMem cpu 4 `shouldBe` 5
      getMem cpu 12 `shouldBe` 11
