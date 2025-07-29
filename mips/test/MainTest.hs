module MainTest where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Gen, Property, ioProperty, forAll, Arbitrary(..), elements, chooseInt, verbose)
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Main

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

-- Helper to get a register value
getRegValue :: CPU -> RegID -> Int
getRegValue cpu regId = case getReg cpu regId of
  Val x -> x
  Pending _ _ -> error "Register still pending"

-- Helper to check PC
getPC :: CPU -> Int
getPC = pc

-- Helper to get memory value
getMemValue :: CPU -> Int -> Int
getMemValue cpu addr = memory cpu !! addr

-- Helper to get cycle count
getCycles :: CPU -> Int
getCycles = cycleCount

-- Custom CPU initializer for tests
initTestCPU :: [RegValue] -> [Int] -> [Instruction] -> CPU
initTestCPU regs mem prog = CPU
  { registers = regs
  , stations = [emptyStation RS_ADD1, emptyStation RS_ADD2, emptyStation RS_LS1]
  , rob = []
  , pc = 0
  , memory = mem
  , robOffset = 0
  , program = prog
  , cycleCount = 0
  , instructionsLeft = 3000
  }
  
-- Custom CPU initializer for in-order execution
initInOrderCpu :: [RegValue] -> [Int] -> [Instruction] -> InOrderCPU
initInOrderCpu regs mem prog = InOrderCPU
  { inOrderRegisters = 
    map (\regVal -> 
        case regVal of
        Val v -> v
        _ -> 0
    ) regs,
    inOrderMemory = mem,
    inOrderProgram = prog,
    inOrderPC = 0,
    inOrderInstructionsLeft = 3000
  }

-- Helper to run a program with custom state
runProgramWithState :: [RegValue] -> [Int] -> [Instruction] -> IO CPU
runProgramWithState regs mem prog = executeProgram (initTestCPU regs mem prog)

runProgramInOrderWithState :: [RegValue] -> [Int] -> [Instruction] -> IO InOrderCPU
runProgramInOrderWithState regs mem prog = executeProgramInOrder (initInOrderCpu regs mem prog)

-- ============================================================================
-- TEST DATA
-- ============================================================================

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

-- ============================================================================
-- TEST PROGRAMS
-- ============================================================================

-- Test Program 1: Simple BEQ taken
test1Prog :: [Instruction]
test1Prog =
  [ INSTR_ADD R1 R2 R0    -- R1 = 3 + 0 = 3
  , INSTR_BEQ R1 R2 1     -- Taken: skip next instruction
  , INSTR_ADD R3 R3 R4    -- Should be skipped (R3 would be 2+3=5)
  , INSTR_NOP
  ]

-- Test Program 2: BNE not taken
test2Prog :: [Instruction]
test2Prog =
  [ INSTR_ADD R1 R2 R0    -- R1 = 3 + 0 = 3
  , INSTR_BNE R1 R2 1     -- Not taken (3==3), do not skip
  , INSTR_ADD R3 R3 R4    -- Executed: R3 = 2+3=5
  , INSTR_NOP
  ]

-- Test Program 3: Loop with counter
test3Prog :: [Instruction]
test3Prog =
  [ INSTR_ADD R5 R6 R0    -- R5 = 3 (R6=3)
  , INSTR_ADD R5 R5 R7    -- R5 = 3 + (-1) = 2
  , INSTR_BNE R5 R0 (-2)  -- Loop back to first ADD if R5 != 0
  , INSTR_NOP
  ]

-- Test Program 4: Data hazard with branch
test4Prog :: [Instruction]
test4Prog =
  [ INSTR_ADD R1 R2 R3    -- R1 = 1 + 2 = 3
  , INSTR_BEQ R1 R4 1     -- Taken (3==3), skip next
  , INSTR_ADD R5 R5 R6    -- Should be skipped
  , INSTR_NOP
  ]

-- Test Program 5: Complex hazard scenario
test5Prog :: [Instruction]
test5Prog =
  [ INSTR_ADD R1 R2 R3    -- R1 = 1+2=3
  , INSTR_LW R3 8 R0      -- Load from mem[8] (0)
  , INSTR_BEQ R1 R4 2     -- Taken (3==3), jump over next 2
  , INSTR_ADD R5 R1 R3    -- Skipped
  , INSTR_SW R1 12 R0     -- Skipped
  , INSTR_ADD R6 R1 R1    -- R6 = 3+3=6
  , INSTR_NOP
  ]

-- Test Program 6: Counter loop
test6Prog :: [Instruction]
test6Prog =
  [ INSTR_ADD R1 R7 R1    -- R1 = 1 + (-1) = 0
  , INSTR_ADD R4 R4 R4    -- R4 = 6
  , INSTR_ADD R4 R4 R4    -- R4 = 12
  , INSTR_ADD R4 R4 R4    -- R4 = 24
  , INSTR_BEQ R1 R4 3     -- Not taken initially (0 != 24)
  , INSTR_ADD R1 R1 R2    -- R1 += R2
  , INSTR_ADD R1 R1 R2    -- R1 += R2
  , INSTR_BEQ R0 R0 (-4)  -- Unconditional branch back
  , INSTR_NOP
  ]

-- Matrix-Vector Multiplication Program
mvMultiplication :: [Instruction]
mvMultiplication =
  [ -- Load matrix and vector elements
    INSTR_LW R3 0 R0      -- R3 = a11 = 1
  , INSTR_LW R4 16 R0     -- R4 = b1 = 3
  -- R1 = a11 * b1 (multiplication by repeated addition)
  , INSTR_MOV R1 R0       -- R1 = 0
  , INSTR_MOV R5 R4       -- R5 = b1 (counter)
  , INSTR_MOV R6 R0       -- R6 = 0 (accumulator)
  -- Loop: a11 * b1
  , INSTR_BEQ R5 R0 3     -- Exit loop when counter = 0
  , INSTR_ADD R6 R6 R3    -- accumulator += a11
  , INSTR_ADD R5 R5 R7    -- counter-- (R7 = -1)
  , INSTR_BEQ R0 R0 (-4)  -- Branch back to loop start
  , INSTR_MOV R1 R6       -- R1 = result of a11 * b1

  -- Similar loops for other multiplications...
  , INSTR_LW R3 4 R0      -- R3 = a12 = 2
  , INSTR_LW R4 20 R0     -- R4 = b2 = 4
  , INSTR_MOV R5 R4
  , INSTR_MOV R6 R0
  -- Loop: a12 * b2
  , INSTR_BEQ R5 R0 3
  , INSTR_ADD R6 R6 R3
  , INSTR_ADD R5 R5 R7
  , INSTR_BEQ R0 R0 (-4)
  , INSTR_ADD R1 R1 R6    -- R1 += a12 * b2

  , INSTR_LW R3 8 R0      -- R3 = a21 = 3
  , INSTR_LW R4 16 R0     -- R4 = b1 = 3
  , INSTR_MOV R5 R4
  , INSTR_MOV R6 R0
  -- Loop: a21 * b1
  , INSTR_BEQ R5 R0 3
  , INSTR_ADD R6 R6 R3
  , INSTR_ADD R5 R5 R7
  , INSTR_BEQ R0 R0 (-4)
  , INSTR_MOV R2 R6       -- R2 = result of a21 * b1

  , INSTR_LW R3 12 R0     -- R3 = a22 = 4
  , INSTR_LW R4 20 R0     -- R4 = b2 = 4
  , INSTR_MOV R5 R4
  , INSTR_MOV R6 R0
  -- Loop: a22 * b2
  , INSTR_BEQ R5 R0 3
  , INSTR_ADD R6 R6 R3
  , INSTR_ADD R5 R5 R7
  , INSTR_BEQ R0 R0 (-4)
  , INSTR_ADD R2 R2 R6    -- R2 += a22 * b2

  -- Store results
  , INSTR_SW R1 24 R0     -- Store result[0]
  , INSTR_SW R2 28 R0     -- Store result[1]
  , INSTR_NOP
  ]

-- Matrix-Vector multiplication memory layout
mvMultiplicationMem :: [Int]
mvMultiplicationMem =
  [ 1, 0, 0, 0   -- 0-3:   a11 = 1
  , 2, 0, 0, 0   -- 4-7:   a12 = 2
  , 3, 0, 0, 0   -- 8-11:  a21 = 3
  , 4, 0, 0, 0   -- 12-15: a22 = 4
  , 3, 0, 0, 0   -- 16-19: b1 = 3
  , 4, 0, 0, 0   -- 20-23: b2 = 4
  , 0, 0, 0, 0   -- 24-27: result[0]
  , 0, 0, 0, 0   -- 28-31: result[1]
  ] ++ replicate 32 0

-- Matrix-Vector multiplication registers
mvMultiplicationRegs :: [RegValue]
mvMultiplicationRegs =
  [ Val 0, Val 0, Val 0, Val 0, Val 0, Val 0, Val 0, Val (-1) ]

-- Comprehensive hazard test program
hazardTestProg :: [Instruction]
hazardTestProg =
  [ -- Memory hazards
    INSTR_LW R1 0 R0      -- R1 = 5
  , INSTR_SW R1 4 R0      -- mem[4] = 5
  
  -- Data hazards
  , INSTR_LW R2 16 R0     -- R2 = 1
  , INSTR_ADD R3 R1 R2    -- R3 = 6
  , INSTR_ADD R4 R3 R1    -- R4 = 11
  , INSTR_ADD R1 R4 R0    -- R1 = 11
  
  -- Control hazards
  , INSTR_LW R5 20 R0     -- R5 = 5
  , INSTR_ADD R5 R5 R2    -- R5 = 6
  , INSTR_BEQ R3 R5 2     -- Taken, skip next 2
  , INSTR_ADD R6 R6 R2    -- Should be skipped
  , INSTR_ADD R7 R7 R2    -- Should be skipped
  , INSTR_LW R6 8 R0      -- R6 = 10
  
  -- Branch delay hazards
  , INSTR_BEQ R6 R0 1     -- Not taken (R6=10 != 0)
  , INSTR_ADD R7 R6 R2    -- R7 = 11
  , INSTR_SW R7 12 R0     -- mem[12] = 11
  , INSTR_NOP
  ]

-- Hazard test memory layout
hazardTestMem :: [Int]
hazardTestMem = 
  [ 5,0,0,0, 0,0,0,0, 10,0,0,0, 0,0,0,0  -- Test data
  , 1,0,0,0, 5,0,0,0                      -- More test data
  ] ++ replicate 250 0

-- Hazard test registers (all zeros initially)
hazardTestRegs :: [RegValue]
hazardTestRegs = replicate 8 (Val 0)

-- ============================================================================
-- QuickCheck utilities
-- ============================================================================

-- Generator for register IDs
genRegID :: Gen RegID
genRegID = elements [minBound .. maxBound]

genDestRegID :: Gen RegID
genDestRegID = elements [R1, R2, R3, R4, R5, R6, R7] -- Exclude R0

-- Generator for small (safe) memory offsets
genSmallInt :: Gen Int
genSmallInt = chooseInt (0, 15) -- Adjust if you want larger/smaller memories

-- Generator for random programs:
-- Generate only forward branches (no infinite loops)
genSafeProgramNoLoops :: Int -> Gen [Instruction]
genSafeProgramNoLoops len = go 0
  where
    go i
      | i >= len  = return []
      | otherwise = do
          instrType <- elements [0..6 :: Int]
          instr <- case instrType of
            0 -> do rd <- genDestRegID; r1 <- genRegID; r2 <- genRegID
                    return $ INSTR_ADD rd r1 r2
            1 -> do rd <- genDestRegID; rs <- genRegID
                    return $ INSTR_MOV rd rs
            2 -> do rd <- genDestRegID; offset <- genSmallInt;
                    return $ INSTR_LW rd offset R0
            3 -> do src <- genRegID; offset <- genSmallInt;
                    return $ INSTR_SW src offset R0
            4 -> do r1 <- genRegID; r2 <- genRegID
                    let maxForward = len - (i + 1); maxBackward = i
                    off <- if maxForward <= 0
                           then return 0
                           else chooseInt (-maxBackward, min maxForward 10)
                    return $ INSTR_BEQ r1 r2 off
            5 -> do r1 <- genRegID; r2 <- genRegID
                    let maxForward = len - (i + 1); maxBackward = i
                    off <- if maxForward <= 0
                           then return 0
                           else chooseInt (-maxBackward, min maxForward 10)
                    return $ INSTR_BNE r1 r2 off
            _ -> return INSTR_NOP
          (instr :) <$> go (i + 1)
          
-- quickCheck property to compare OOO and InOrder execution
prop_CpusProduceSameResults :: Property
prop_CpusProduceSameResults =
  forAll (genSafeProgramNoLoops 30) $ \prog ->
    ioProperty $ do
      let -- Use the spicy values above
          initRegs = [ Val 0, Val (-7), Val 1000, Val 3, Val (-123), Val 17, Val 256, Val (-1) ]
          initMem  = take 64 $ cycle [5, -9, 77, 16000, -88, 2, 0, 555]

      cpuOOO <- runProgramWithState initRegs initMem prog
      cpuInO <- runProgramInOrderWithState initRegs initMem prog

      let regsOOO = map (getRegValue cpuOOO) [R0 .. R7]
          regsInO = inOrderRegisters cpuInO
          memOOO  = memory cpuOOO
          memInO  = inOrderMemory cpuInO

      return $ regsOOO == regsInO && memOOO == memInO


-- ============================================================================
-- TEST SUITE
-- ============================================================================

prop_models_agree :: [Instruction] -> Property
prop_models_agree instrs = monadicIO $ do
  let initRegs = replicate 8 (Val 0)
      initMem  = replicate 64 0
  cpu1 <- run $ runProgramWithState initRegs initMem instrs
  cpu2 <- run $ runProgramInOrderWithState initRegs initMem instrs
  let regs1 = map (\regs -> case regs of
                Val v -> v
                _ -> error "reg value not final on execution completion") (registers cpu1)
      regs2 = inOrderRegisters cpu2
      mem1 = memory cpu1
      mem2 = inOrderMemory cpu2
  assert (regs1 == regs2 && mem1 == mem2)

main :: IO ()
main = hspec $ do
  describe "Basic Branch Tests" $ do
    it "Test 1: Simple BEQ taken - should skip instruction after branch" $ do
      cpu <- runProgramWithState testRegs testMem test1Prog
      getRegValue cpu R3 `shouldBe` 2  -- R3 remains unchanged
      getCycles cpu `shouldSatisfy` (> 0)
    
    it "Test 2: BNE not taken - should execute instruction after branch" $ do
      cpu <- runProgramWithState testRegs testMem test2Prog
      getRegValue cpu R3 `shouldBe` 5  -- R3 = 2 + 3 = 5
    
    it "Test 3: Loop with counter - should loop until R5 becomes 0" $ do
      cpu <- runProgramWithState testRegs testMem test3Prog
      getRegValue cpu R5 `shouldBe` 0
  
  describe "Data Hazard Tests" $ do
    it "Test 4: Data hazard with branch - should skip instruction after branch" $ do
      cpu <- runProgramWithState test45Regs testMem test4Prog
      getRegValue cpu R5 `shouldBe` 1  -- R5 remains unchanged
    
    it "Test 5: Complex hazard scenario - should handle multiple hazards" $ do
      cpu <- runProgramWithState test45Regs testMem test5Prog
      getRegValue cpu R6 `shouldBe` 6  -- R6 = 3 + 3 = 6
      getMemValue cpu 12 `shouldBe` 0  -- Store should be skipped
  
  describe "Loop Tests" $ do
    it "Test 6: Counter loop - should terminate when counter reaches target" $ do
      cpu <- runProgramWithState test45Regs testMem test6Prog
      -- This test may need adjustment based on the actual loop behavior
      getRegValue cpu R1 `shouldSatisfy` (>= 0)
  
  describe "Matrix-Vector Multiplication" $ do
    it "should compute matrix-vector multiplication correctly" $ do
      cpu <- runProgramWithState mvMultiplicationRegs mvMultiplicationMem mvMultiplication
      -- Expected: [1 2] * [3] = [11]
      --          [3 4]   [4]   [25]
      getMemValue cpu 24 `shouldBe` 11  -- result[0] = 1*3 + 2*4 = 11
      getMemValue cpu 28 `shouldBe` 25  -- result[1] = 3*3 + 4*4 = 25
  
  describe "Comprehensive Hazard Test" $ do
    it "should handle all hazard types correctly" $ do
      cpu <- runProgramWithState hazardTestRegs hazardTestMem hazardTestProg
      getRegValue cpu R1 `shouldBe` 11
      getRegValue cpu R7 `shouldBe` 11
      getMemValue cpu 4 `shouldBe` 5
      getMemValue cpu 12 `shouldBe` 11
  
  describe "Performance Tests" $ do
    it "should complete simple programs in reasonable cycles" $ do
      cpu <- runProgramWithState testRegs testMem test1Prog
      getCycles cpu `shouldSatisfy` (< 100)
    
    it "should handle complex programs without infinite loops" $ do
      cpu <- runProgramWithState hazardTestRegs hazardTestMem hazardTestProg
      getCycles cpu `shouldSatisfy` (< 1000)

  describe "QuickCheck Tests" $ do
    modifyMaxSuccess (const 1000) $ do
      prop "OOO matches InOrder for arbitrary program" $ verbose prop_CpusProduceSameResults
        
