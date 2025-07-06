{-# LANGUAGE RecordWildCards #-}
module Main where

import Debug.Trace
import Data.List (find, elemIndex)
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)

-- ============================================================================
-- CORE DATA TYPES
-- ============================================================================

data Instruction =
    INSTR_ADD RegID RegID RegID
  | INSTR_MOV RegID RegID
  | INSTR_LW RegID Int RegID
  | INSTR_SW RegID Int RegID
  | INSTR_BEQ RegID RegID Int
  | INSTR_BNE RegID RegID Int
  | INSTR_NOP
  deriving (Show, Eq)

data Operation = OP_ADD | OP_LW | OP_SW | OP_BEQ | OP_BNE deriving (Show, Eq)

data RegID = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 deriving (Show, Eq, Enum, Bounded)

data RegValue = Val Int | Pending Int Int deriving (Show, Eq)

data RSID = RS_ADD1 | RS_ADD2 | RS_LS1 deriving (Show, Eq, Enum, Bounded)

-- Simplified ROB Entry
data ROBEntry = ROBEntry {
    robTag     :: Int,
    robInstr   :: Instruction,
    robDest    :: Maybe RegID,
    robResult  :: Maybe Int,
    robAddr    :: Maybe Int,  -- For stores
    robReady   :: Bool
} deriving (Show, Eq)

-- Simplified Reservation Station
data ResStation = ResStation {
    rsid       :: RSID,
    operation  :: Operation,
    operand1   :: Maybe Int,    -- Nothing means waiting
    operand2   :: Maybe Int,    -- Nothing means waiting
    waitTag1   :: Maybe Int,    -- ROB tag we're waiting for
    waitTag2   :: Maybe Int,    -- ROB tag we're waiting for
    immediate  :: Int,          -- For loads/stores/branches
    busy       :: Bool,
    destTag    :: Maybe Int     -- ROB tag this writes to
} deriving (Show, Eq)

data CPU = CPU {
    registers   :: [RegValue],
    stations    :: [ResStation],
    rob         :: [ROBEntry],
    pc          :: Int,
    memory      :: [Int],
    robOffset   :: Int,
    program     :: [Instruction],
    cycleCount  :: Int
} deriving (Show)

-- ============================================================================
-- CONFIGURATION & INITIALIZATION
-- ============================================================================

initialMemory :: [Int]
initialMemory = [1,0,0,0, 2,0,0,0, 3,0,0,0, 4,0,0,0,  -- Matrix A
                 3,0,0,0, 4,0,0,0,                      -- Vector B
                 0,0,0,0, 0,0,0,0]                      -- Results

initialRegisters :: [RegValue]
initialRegisters = [Val 0, Val 0, Val 0, Val 0, Val 0, Val 0, Val 0, Val (-1)]

emptyStation :: RSID -> ResStation
emptyStation rsid = ResStation {
    rsid = rsid,
    operation = OP_ADD,
    operand1 = Nothing,
    operand2 = Nothing,
    waitTag1 = Nothing,
    waitTag2 = Nothing,
    immediate = 0,
    busy = False,
    destTag = Nothing
}

initCPU :: [Instruction] -> CPU
initCPU prog = CPU {
    registers = initialRegisters,
    stations = [emptyStation RS_ADD1, emptyStation RS_ADD2, emptyStation RS_LS1],
    rob = [],
    pc = 0,
    memory = initialMemory,
    robOffset = 0,
    program = prog,
    cycleCount = 0
}

-- ============================================================================
-- UTILITY FUNCTIONS
-- ============================================================================

getReg :: CPU -> RegID -> RegValue
getReg cpu regId = registers cpu !! fromEnum regId

setReg :: CPU -> RegID -> RegValue -> CPU
setReg cpu regId val = cpu { registers = updateList (registers cpu) (fromEnum regId) val }

updateList :: [a] -> Int -> a -> [a]
updateList xs i x = take i xs ++ [x] ++ drop (i + 1) xs

findFreeStation :: CPU -> Operation -> Maybe RSID
findFreeStation cpu op = case op of
    OP_ADD -> find (\rsid -> not $ busy $ stations cpu !! fromEnum rsid) [RS_ADD1, RS_ADD2]
    OP_BEQ -> find (\rsid -> not $ busy $ stations cpu !! fromEnum rsid) [RS_ADD1, RS_ADD2]
    OP_BNE -> find (\rsid -> not $ busy $ stations cpu !! fromEnum rsid) [RS_ADD1, RS_ADD2]
    OP_LW  -> if not (busy $ stations cpu !! fromEnum RS_LS1) then Just RS_LS1 else Nothing
    OP_SW  -> if not (busy $ stations cpu !! fromEnum RS_LS1) then Just RS_LS1 else Nothing

nextROBTag :: CPU -> Int
nextROBTag cpu = robOffset cpu + length (rob cpu)

-- ============================================================================
-- REGISTER DEPENDENCY RESOLUTION
-- ============================================================================

data OperandInfo = OperandInfo {
    value :: Maybe Int,
    waitingFor :: Maybe Int
} deriving (Show)

resolveOperand :: CPU -> RegID -> OperandInfo
resolveOperand cpu regId = case getReg cpu regId of
    Val v -> OperandInfo (Just v) Nothing
    Pending tag oldVal -> OperandInfo Nothing (Just tag)

-- ============================================================================
-- INSTRUCTION ISSUE PHASE
-- ============================================================================

getDestReg :: Instruction -> Maybe RegID
getDestReg instr = case instr of
    INSTR_ADD rd _ _ -> Just rd
    INSTR_LW rd _ _  -> Just rd
    _               -> Nothing

getOperation :: Instruction -> Operation
getOperation instr = case instr of
    INSTR_ADD _ _ _ -> OP_ADD
    INSTR_LW _ _ _  -> OP_LW
    INSTR_SW _ _ _  -> OP_SW
    INSTR_BEQ _ _ _ -> OP_BEQ
    INSTR_BNE _ _ _ -> OP_BNE
    _             -> error "Invalid operation"

createROBEntry :: CPU -> Instruction -> Int -> ROBEntry
createROBEntry cpu instr tag = ROBEntry {
    robTag = tag,
    robInstr = instr,
    robDest = getDestReg instr,
    robResult = Nothing,
    robAddr = Nothing,
    robReady = False
}

createResStation :: CPU -> Instruction -> RSID -> Int -> ResStation
createResStation cpu instr rsid robTag = case instr of
    INSTR_ADD _ r1 r2 -> 
        let op1 = resolveOperand cpu r1
            op2 = resolveOperand cpu r2
        in ResStation {
            rsid = rsid,
            operation = OP_ADD,
            operand1 = value op1,
            operand2 = value op2,
            waitTag1 = waitingFor op1,
            waitTag2 = waitingFor op2,
            immediate = 0,
            busy = True,
            destTag = Just robTag
        }
    
    INSTR_LW _ offset baseReg ->
        let baseOp = resolveOperand cpu baseReg
        in ResStation {
            rsid = rsid,
            operation = OP_LW,
            operand1 = Nothing,
            operand2 = value baseOp,
            waitTag1 = Nothing,
            waitTag2 = waitingFor baseOp,
            immediate = offset,
            busy = True,
            destTag = Just robTag
        }
    
    INSTR_SW srcReg offset baseReg ->
        let srcOp = resolveOperand cpu srcReg
            baseOp = resolveOperand cpu baseReg
        in ResStation {
            rsid = rsid,
            operation = OP_SW,
            operand1 = value srcOp,
            operand2 = value baseOp,
            waitTag1 = waitingFor srcOp,
            waitTag2 = waitingFor baseOp,
            immediate = offset,
            busy = True,
            destTag = Just robTag
        }
    
    INSTR_BEQ r1 r2 offset ->
        let op1 = resolveOperand cpu r1
            op2 = resolveOperand cpu r2
        in ResStation {
            rsid = rsid,
            operation = OP_BEQ,
            operand1 = value op1,
            operand2 = value op2,
            waitTag1 = waitingFor op1,
            waitTag2 = waitingFor op2,
            immediate = offset + pc cpu + 1,
            busy = True,
            destTag = Just robTag
        }
    
    INSTR_BNE r1 r2 offset ->
        let op1 = resolveOperand cpu r1
            op2 = resolveOperand cpu r2
        in ResStation {
            rsid = rsid,
            operation = OP_BNE,
            operand1 = value op1,
            operand2 = value op2,
            waitTag1 = waitingFor op1,
            waitTag2 = waitingFor op2,
            immediate = offset + pc cpu + 1,
            busy = True,
            destTag = Just robTag
        }
    
    _ -> error "Cannot create reservation station for this instruction"

issueInstruction :: CPU -> Instruction -> Maybe CPU
issueInstruction cpu instr = case instr of
    INSTR_NOP -> Just cpu { pc = pc cpu + 1 }
    
    INSTR_MOV dst src -> 
        let srcVal = getReg cpu src
            newCpu = setReg cpu dst srcVal
        in Just newCpu { pc = pc cpu + 1 }
    
    _ -> case findFreeStation cpu (getOperation instr) of
        Nothing -> Nothing  -- Structural hazard
        Just rsid -> 
            let tag = nextROBTag cpu
                robEntry = createROBEntry cpu instr tag
                station = createResStation cpu instr rsid tag
                newROB = rob cpu ++ [robEntry]
                newStations = updateList (stations cpu) (fromEnum rsid) station
                newRegs = case getDestReg instr of
                    Nothing -> registers cpu
                    Just destReg -> updateList (registers cpu) (fromEnum destReg) 
                                   (Pending tag (case getReg cpu destReg of Val v -> v; Pending _ v -> v))
            in Just cpu {
                rob = newROB,
                stations = newStations,
                registers = newRegs,
                pc = pc cpu + 1
            }

-- ============================================================================
-- EXECUTION PHASE
-- ============================================================================

canExecute :: ResStation -> Bool
canExecute station = busy station && isNothing (waitTag1 station) && isNothing (waitTag2 station)

executeStation :: CPU -> ResStation -> Maybe (Int, Maybe Int)  -- (result, store_address)
executeStation cpu station = case operation station of
    OP_ADD -> 
        let result = fromJust (operand1 station) + fromJust (operand2 station)
        in Just (result, Nothing)
    
    OP_LW -> 
        let addr = immediate station + fromJust (operand2 station)
        in if addr >= 0 && addr < length (memory cpu)
           then Just (memory cpu !! addr, Nothing)
           else error $ "Load address out of bounds: " ++ show addr
    
    OP_SW -> 
        let addr = immediate station + fromJust (operand2 station)
            val = fromJust (operand1 station)
        in if addr >= 0 && addr < length (memory cpu)
           then Just (val, Just addr)
           else error $ "Store address out of bounds: " ++ show addr
    
    OP_BEQ -> 
        let taken = fromJust (operand1 station) == fromJust (operand2 station)
        in if taken then Just (immediate station, Nothing) else Just (0, Nothing)
    
    OP_BNE -> 
        let taken = fromJust (operand1 station) /= fromJust (operand2 station)
        in if taken then Just (immediate station, Nothing) else Just (0, Nothing)

updateROBWithResult :: CPU -> Int -> Int -> Maybe Int -> CPU
updateROBWithResult cpu robTag result storeAddr = 
    let robIndex = robTag - robOffset cpu
        updateEntry entry = entry { robResult = Just result, robAddr = storeAddr, robReady = True }
        newROB = updateList (rob cpu) robIndex (updateEntry (rob cpu !! robIndex))
    in cpu { rob = newROB }

forwardResult :: CPU -> Int -> Int -> CPU
forwardResult cpu robTag result = 
    let updateStation station = 
            let station' = if waitTag1 station == Just robTag 
                          then station { operand1 = Just result, waitTag1 = Nothing }
                          else station
                station'' = if waitTag2 station' == Just robTag 
                           then station' { operand2 = Just result, waitTag2 = Nothing }
                           else station'
            in station''
        newStations = map updateStation (stations cpu)
    in cpu { stations = newStations }

executeAllStations :: CPU -> CPU
executeAllStations cpu = 
    let executeOne cpu' station = 
            if canExecute station
            then case executeStation cpu' station of
                Just (result, storeAddr) -> 
                    let robTag = fromJust (destTag station)
                        cpu1 = updateROBWithResult cpu' robTag result storeAddr
                        cpu2 = forwardResult cpu1 robTag result
                        clearedStation = station { busy = False, destTag = Nothing }
                        cpu3 = cpu2 { stations = updateList (stations cpu2) (fromEnum (rsid station)) clearedStation }
                    in cpu3
                Nothing -> cpu'
            else cpu'
    in foldl executeOne cpu (stations cpu)

-- ============================================================================
-- COMMIT PHASE
-- ============================================================================

commitInstruction :: CPU -> ROBEntry -> CPU
commitInstruction cpu entry = case robInstr entry of
    INSTR_ADD _ _ _ -> commitRegisterWrite cpu entry
    INSTR_LW _ _ _  -> commitRegisterWrite cpu entry
    INSTR_SW _ _ _  -> commitStore cpu entry
    INSTR_BEQ _ _ _ -> commitBranch cpu entry
    INSTR_BNE _ _ _ -> commitBranch cpu entry
    _ -> advanceROB cpu

commitRegisterWrite :: CPU -> ROBEntry -> CPU
commitRegisterWrite cpu entry = 
    let result = fromJust (robResult entry)
        updateRegs = case robDest entry of
            Nothing -> registers cpu
            Just destReg -> updateList (registers cpu) (fromEnum destReg) (Val result)
        cpu1 = cpu { registers = updateRegs }
        cpu2 = forwardResult cpu1 (robTag entry) result
    in advanceROB cpu2

commitStore :: CPU -> ROBEntry -> CPU
commitStore cpu entry = 
    let addr = fromJust (robAddr entry)
        val = fromJust (robResult entry)
        newMem = updateList (memory cpu) addr val
    in advanceROB cpu { memory = newMem }

commitBranch :: CPU -> ROBEntry -> CPU
commitBranch cpu entry = case robResult entry of
    Just targetPC | targetPC /= 0 -> -- Branch taken (misprediction)
        let speculativeEntries = tail (rob cpu)
            speculativeTags = map robTag speculativeEntries
            revertedRegs = revertSpeculativeRegs (registers cpu) speculativeTags
            clearedStations = map clearStation (stations cpu)
        in cpu {
            rob = [],
            registers = revertedRegs,
            stations = clearedStations,
            pc = targetPC,
            robOffset = robOffset cpu + length (rob cpu)
        }
    _ -> advanceROB cpu -- Branch not taken (correct prediction)

revertSpeculativeRegs :: [RegValue] -> [Int] -> [RegValue]
revertSpeculativeRegs regs speculativeTags = 
    map (\regVal -> case regVal of
        Pending tag oldVal -> if tag `elem` speculativeTags then Val oldVal else Pending tag oldVal
        Val v -> Val v
    ) regs

clearStation :: ResStation -> ResStation
clearStation station = station { busy = False, destTag = Nothing, waitTag1 = Nothing, waitTag2 = Nothing }

advanceROB :: CPU -> CPU
advanceROB cpu = cpu { rob = tail (rob cpu), robOffset = robOffset cpu + 1 }

commitROB :: CPU -> CPU
commitROB cpu = case rob cpu of
    [] -> cpu
    (entry:_) -> if robReady entry 
                then commitInstruction cpu entry
                else cpu

-- ============================================================================
-- MAIN EXECUTION LOOP
-- ============================================================================

cpuCycle :: CPU -> CPU
cpuCycle cpu = 
    let cpu1 = cpu { cycleCount = cycleCount cpu + 1 }
        cpu2 = executeAllStations cpu1
        cpu3 = commitROB cpu2
    in cpu3

executeProgram :: CPU -> IO CPU
executeProgram cpu
    | pc cpu >= length (program cpu) && null (rob cpu) && all (not . busy) (stations cpu) = 
        return cpu
    | cycleCount cpu > 10000 = do  -- Safety limit
        putStrLn "Execution limit reached - possible infinite loop"
        return cpu
    | otherwise = do
        -- Try to issue next instruction
        newCpu <- if pc cpu < length (program cpu)
                 then case issueInstruction cpu (program cpu !! pc cpu) of
                     Just cpu' -> return cpu'
                     Nothing -> return cpu  -- Structural hazard, try next cycle
                 else return cpu
        
        -- Execute one cycle
        let cycledCpu = cpuCycle newCpu
        
        -- Continue
        executeProgram cycledCpu

-- ============================================================================
-- TEST PROGRAM
-- ============================================================================

testProgram :: [Instruction]
testProgram = [
    INSTR_LW R1 0 R0,      -- Load 1
    INSTR_LW R2 4 R0,      -- Load 2  
    INSTR_ADD R3 R1 R2,    -- Add them
    INSTR_SW R3 24 R0,     -- Store result
    INSTR_NOP
]

main :: IO ()
main = do
    putStrLn "Starting MIPS Tomasulo Simulation"
    finalCPU <- executeProgram (initCPU testProgram)
    putStrLn $ "Execution completed in " ++ show (cycleCount finalCPU) ++ " cycles"
    putStrLn $ "Final memory: " ++ show (take 8 (memory finalCPU))
    putStrLn $ "Final registers: " ++ show (registers finalCPU)
