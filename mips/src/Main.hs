module Main ( main
               , Instruction(..)
               , RegID(..)
               , RegValue(..)
               , RSID(..)
               , ROBEntry(..)
               , ResStation(..)
               , CPU(..)
               , initCPU
               , executeInstructions
               , mapRegIDToValue
               , Operation(..)
              ) where
import Debug.Trace

-- MIPS instructions (add, mov, lw, sw, beq, bne, nop)
data Instruction =
    INSTR_ADD RegID RegID RegID
  | INSTR_MOV RegID RegID
  | INSTR_LW RegID Int RegID
  | INSTR_SW RegID Int RegID
  | INSTR_BEQ RegID RegID Int
  | INSTR_BNE RegID RegID Int
  | INSTR_NOP
  deriving (Show, Eq)

data Operation = OP_ADD | OP_LW | OP_SW | OP_MV | OP_BEQ | OP_BNE deriving (Show, Eq, Enum)

data RegID = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 deriving (Show, Eq, Enum)

data RegValue = Val Int | Pending Int Int deriving (Show, Eq)

data RSID = RS_ADD1 | RS_ADD2 | RS_LS1 deriving (Show, Eq, Enum)

data ROBEntry = ROBEntry {
    robTag   :: Int,            -- Absolute ROB tag
    robInstr :: Instruction,
    robDest  :: Maybe RegID,    -- Register destination
    robAddr  :: Maybe Int,      -- Memory address (for stores/loads)
    robValue :: Maybe Int,      -- Value/result (if ready)
    robReady :: Bool            -- Is result ready?
} deriving (Show, Eq)

data ResStation = ResStation {
    rsid     :: RSID,
    op       :: Operation,
    vj       :: Int,
    vk       :: Int,
    qj       :: Maybe Int,      -- Source 1: waiting for ROB tag
    qk       :: Maybe Int,      -- Source 2: waiting for ROB tag
    a        :: Int,            -- For loads/stores/branches
    busy     :: Bool,
    robTagRS :: Maybe Int       -- Which ROB entry does this station write to?
} deriving (Show, Eq)

data CPU = CPU {
    reg      :: [RegValue],
    stations :: [ResStation],
    rob      :: [ROBEntry],
    pc       :: Int,            -- Current program counter
    mem      :: [Int],
    robOffset :: Int,           -- Absolute index of the head of the ROB
    program  :: [Instruction]   -- Program storage
} deriving (Show)
  
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

chooseStation :: CPU -> Instruction -> Maybe RSID
chooseStation cpu (INSTR_ADD _ _ _) =
  if not (busy (stations cpu !! fromEnum RS_ADD1))
  then Just RS_ADD1
  else if not (busy (stations cpu !! fromEnum RS_ADD2))
       then Just RS_ADD2
       else Nothing
chooseStation cpu (INSTR_LW _ _ _) =
  if not (busy (stations cpu !! fromEnum RS_LS1))
  then Just RS_LS1
  else Nothing
chooseStation cpu (INSTR_SW _ _ _) =
  if not (busy (stations cpu !! fromEnum RS_LS1))
  then Just RS_LS1
  else Nothing
chooseStation cpu (INSTR_BEQ _ _ _) =
  if not (busy (stations cpu !! fromEnum RS_ADD1))
  then Just RS_ADD1
  else if not (busy (stations cpu !! fromEnum RS_ADD2))
       then Just RS_ADD2
       else Nothing
chooseStation cpu (INSTR_BNE _ _ _) =
  if not (busy (stations cpu !! fromEnum RS_ADD1))
  then Just RS_ADD1
  else if not (busy (stations cpu !! fromEnum RS_ADD2))
       then Just RS_ADD2
       else Nothing
chooseStation _ _ = error "Invalid instruction for reservation station"

allocateROBEntry :: CPU -> Instruction -> Maybe RegID -> (CPU, Int)
allocateROBEntry cpu instr dest =
  let tag = robOffset cpu + length (rob cpu)
      entry = ROBEntry tag instr dest Nothing Nothing False
      newROB = rob cpu ++ [entry]
  in (cpu { rob = newROB }, tag)

issueInstruction :: CPU -> Instruction -> IO (Maybe CPU)
issueInstruction cpu instr = do
  putStrLn ("Issuing instruction " ++ show instr)
  case chooseStation cpu instr of
    Just stationID -> do
      let destReg = case instr of
                      INSTR_ADD rd _ _ -> Just rd
                      INSTR_LW rd _ _   -> Just rd
                      _                -> Nothing
          (cpuWithROB, absTag) = allocateROBEntry cpu instr destReg
          newStation = case instr of
            INSTR_ADD _ r1 r2  -> createAddStation cpuWithROB r1 r2 stationID (Just absTag)
            INSTR_LW _ _ _       -> createLoadStation cpuWithROB instr stationID (Just absTag)
            INSTR_SW _ _ _       -> createStoreStation cpuWithROB instr stationID (Just absTag)
            INSTR_BEQ r1 r2 ofs-> createBranchStation cpuWithROB r1 r2 (ofs + pc cpuWithROB) stationID (Just absTag) True
            INSTR_BNE r1 r2 ofs-> createBranchStation cpuWithROB r1 r2 (ofs + pc cpuWithROB) stationID (Just absTag) False
            _                  -> error "Invalid instruction for reservation station"
          newStations = writeToSeq (stations cpuWithROB) (fromEnum stationID) newStation
          newRegs = case destReg of
            Just destRegId -> updateRegs (reg cpuWithROB) destRegId absTag
            Nothing        -> reg cpuWithROB
          newCpu = cpuWithROB { stations = newStations, reg = newRegs }
      putStrLn ("Issued to station " ++ show stationID ++ " and ROB tag " ++ show absTag)
      return (Just newCpu)
    Nothing -> do
      putStrLn "No available reservation stations for operation"
      return Nothing

createAddStation :: CPU -> RegID -> RegID -> RSID -> Maybe Int -> ResStation
createAddStation cpu r1 r2 selfRsid destRobTag = ResStation {
  rsid = selfRsid,
  op = OP_ADD,
  vj = case state1 of Val v -> v; Pending _ _ -> 0,
  vk = case state2 of Val v -> v; Pending _ _ -> 0,
  qj = case state1 of Val _ -> Nothing; Pending robTag' _ -> Just robTag',
  qk = case state2 of Val _ -> Nothing; Pending robTag' _ -> Just robTag',
  a = 0,
  busy = True,
  robTagRS = destRobTag
} where 
    state1 = mapRegIDToValue cpu r1
    state2 = mapRegIDToValue cpu r2

createLoadStation :: CPU -> Instruction -> RSID -> Maybe Int -> ResStation
createLoadStation cpu (INSTR_LW r1 srcImm srcReg) selfRsid destRobTag = ResStation {
  rsid = selfRsid,
  op = OP_LW,
  qj = case state1 of Val _ -> Nothing; Pending robTag' _ -> Just robTag',
  qk = case state2 of Val _ -> Nothing; Pending robTag' _ -> Just robTag',
  vj = case state1 of Val v -> v; Pending _ _ -> 0,
  vk = case state2 of Val v -> v; Pending _ _ -> 0,
  a = srcImm,
  busy = True,
  robTagRS = destRobTag
} where 
    state1 = mapRegIDToValue cpu r1
    state2 = mapRegIDToValue cpu srcReg
createLoadStation _ _ _ _ = error "Invalid instruction for load station"

createStoreStation :: CPU -> Instruction -> RSID -> Maybe Int -> ResStation
createStoreStation cpu (INSTR_SW r1 destImm destReg) selfRsid destRobTag = ResStation {
  rsid = selfRsid,
  op = OP_SW,
  qj = case state1 of Val _ -> Nothing; Pending robTag' _ -> Just robTag',
  qk = case state2 of Val _ -> Nothing; Pending robTag' _ -> Just robTag',
  vj = case state1 of Val v -> v; Pending _ _ -> 0,
  vk = case state2 of Val v -> v; Pending _ _ -> 0,
  a = destImm,
  busy = True,
  robTagRS = destRobTag
} where 
    state1 = mapRegIDToValue cpu r1
    state2 = mapRegIDToValue cpu destReg
createStoreStation _ _ _ _ = error "Invalid instruction for store station"

createBranchStation :: CPU -> RegID -> RegID -> Int -> RSID -> Maybe Int -> Bool -> ResStation
createBranchStation cpu r1 r2 dest selfRsid destRobTag shouldEqual = ResStation {
  rsid = selfRsid,
  op = case shouldEqual of True -> OP_BEQ; False -> OP_BNE,  -- For simplicity, use OP_BEQ for both (adjust as needed)
  vj = case state1 of Val v -> v; Pending _ _ -> 0,
  vk = case state2 of Val v -> v; Pending _ _ -> 0,
  qj = case state1 of Val _ -> Nothing; Pending robTag' _ -> Just robTag',
  qk = case state2 of Val _ -> Nothing; Pending robTag' _ -> Just robTag',
  a = dest,
  busy = True,
  robTagRS = destRobTag
} where
  state1 = mapRegIDToValue cpu r1
  state2 = mapRegIDToValue cpu r2

runReservationStations :: CPU -> CPU
runReservationStations cpu =
  foldl (\c stn -> if busy stn then executeAndWrite c stn else c) cpu (stations cpu)

executeAndWrite :: CPU -> ResStation -> CPU
executeAndWrite cpu station
  | not (busy station) = cpu
  | otherwise = case op station of
      OP_ADD -> executeAdd cpu station
      OP_LW  -> executeLoad cpu station
      OP_SW  -> executeStore cpu station
      OP_BEQ -> executeBranch cpu station
      OP_BNE -> executeBranch cpu station
      _      -> cpu

executeAdd :: CPU -> ResStation -> CPU
executeAdd cpu station
  | qj station == Nothing && qk station == Nothing =
      let result = vj station + vk station
          absTag = case robTagRS station of Just t -> t; Nothing -> error "No ROB tag"
          newROB = updateROBEntry (rob cpu) absTag (Just result) True (robOffset cpu)
          newStations = overwritePendingResStations (stations cpu) absTag result
          clearedStations = writeToSeq newStations (fromEnum (rsid station)) (station { busy = False })
      in trace ("add ready for execution, ROB tag " ++ show absTag ++ " result " ++ show result) cpu { rob = newROB, stations = clearedStations }
  | otherwise = cpu

executeLoad :: CPU -> ResStation -> CPU
executeLoad cpu station
  | qj station == Nothing && qk station == Nothing =
      let loadAddr = a station + vk station
          result = mem cpu !! loadAddr
          absTag = case robTagRS station of Just t -> t; Nothing -> error "No ROB tag"
          newROB = updateROBEntry (rob cpu) absTag (Just result) True (robOffset cpu)
          newStations = overwritePendingResStations (stations cpu) absTag result
          clearedStations = writeToSeq newStations (fromEnum (rsid station)) (station { busy = False })
      in trace ("load ready for execution, ROB tag " ++ show absTag ++ " result " ++ show result) cpu { rob = newROB, stations = clearedStations }
  | otherwise = cpu

executeStore :: CPU -> ResStation -> CPU
executeStore cpu station
  | qj station == Nothing && qk station == Nothing =
      let storeAddr = a station + vk station
          storeVal = vj station
          absTag = case robTagRS station of Just t -> t; Nothing -> error "No ROB tag"
          newROB = updateROBEntryStore (rob cpu) absTag storeAddr storeVal True (robOffset cpu)
      in trace ("store ready for execution, ROB tag " ++ show absTag ++ ", pending store: " ++ show storeVal ++ " at address " ++ show storeAddr) cpu { rob = newROB }
  | otherwise = cpu

executeBranch :: CPU -> ResStation -> CPU
executeBranch cpu station
  | qj station == Nothing && qk station == Nothing =
      let r1 = vj station
          r2 = vk station
          taken = case op station of
                    OP_BEQ -> r1 == r2
                    OP_BNE -> r1 /= r2
                    _ -> error "Invalid operation for branch"
          absTag = case robTagRS station of Just t -> t; Nothing -> error "No ROB tag"
          newROB = updateROBEntry (rob cpu) absTag (if taken then Just (a station) else Nothing) True (robOffset cpu)
          clearedStations = writeToSeq (stations cpu) (fromEnum (rsid station)) (station { busy = False })
      in trace ("branch ready for execution, ROB tag " ++ show absTag ++ ", taken: " ++ show taken ++ ", destination: " ++ show (a station)) cpu { rob = newROB, stations = clearedStations }
  | otherwise = cpu

overwritePendingRegs :: [RegValue] -> Int -> Int -> [RegValue]
overwritePendingRegs regs absTag result =
  map (\x -> case x of
    Pending tag _ -> if tag == absTag then Val result else x
    _ -> x) regs

overwritePendingResStations :: [ResStation] -> Int -> Int -> [ResStation]
overwritePendingResStations resStations absTag result =
  map (\x ->
    let x'  = if qj x == Just absTag then x { vj = result, qj = Nothing } else x
        x'' = if qk x' == Just absTag then x' { vk = result, qk = Nothing } else x'
    in x''
  ) resStations

updateROBEntry :: [ROBEntry] -> Int -> Maybe Int -> Bool -> Int -> [ROBEntry]
updateROBEntry entries absTag val ready robOffset' =
  let idx = absTag - robOffset'
  in if idx < 0 || idx >= length entries
     then error $ "ROB tag " ++ show absTag ++ " out of bounds (offset " ++ show robOffset' ++ ", len " ++ show (length entries) ++ ")"
     else take idx entries ++
          [ (entries !! idx) { robValue = val, robReady = ready } ] ++
          drop (idx + 1) entries

updateROBEntryStore :: [ROBEntry] -> Int -> Int -> Int -> Bool -> Int -> [ROBEntry]
updateROBEntryStore entries absTag addr val ready robOffset' =
  let idx = absTag - robOffset'
  in if idx < 0 || idx >= length entries
     then error $ "ROB tag " ++ show absTag ++ " out of bounds (offset " ++ show robOffset' ++ ", len " ++ show (length entries) ++ ")"
     else take idx entries ++
          [ (entries !! idx) { robAddr = Just addr, robValue = Just val, robReady = ready } ] ++
          drop (idx + 1) entries

commitROB :: CPU -> CPU
commitROB cpu =
  case rob cpu of
    [] -> cpu
    (entry:rest) ->
      if robReady entry
      then case robInstr entry of
        INSTR_ADD _ _ _ ->
          let newRegs = overwritePendingRegs (reg cpu) (robTag entry) (unwrap (robValue entry))
              newStations = overwritePendingResStations (stations cpu) (robTag entry) (unwrap (robValue entry))
              newCpu = cpu { reg = newRegs, stations = newStations, rob = rest, robOffset = robOffset cpu + 1 }
          in trace ("committed load: " ++ show newCpu) newCpu
        INSTR_LW _ _ _ ->
          let newRegs = overwritePendingRegs (reg cpu) (robTag entry) (unwrap (robValue entry))
              newStations = overwritePendingResStations (stations cpu) (robTag entry) (unwrap (robValue entry))
              newCpu = cpu { reg = newRegs, stations = newStations, rob = rest, robOffset = robOffset cpu + 1 }
          in trace ("committed load: " ++ show newCpu) newCpu
        INSTR_SW _ _ _ ->
          let addr = unwrap (robAddr entry)
              val  = unwrap (robValue entry)
              newMem = writeToSeq (mem cpu) addr val
              newStations = freeStoreStation (stations cpu) (robTag entry)
              newCpu = cpu { mem = newMem, stations = newStations, rob = rest, robOffset = robOffset cpu + 1 }
          in trace ("committed store: " ++ show newCpu) newCpu
        INSTR_BEQ _ _ _ ->
          case robValue entry of
            Just newPc ->  -- Branch taken: flush ROB, reset stations, update PC
              let flushedCPU = cpu {
                    pc = newPc,
                    rob = [],
                    robOffset = robOffset cpu + length (rob cpu),
                    stations = map clearStation (stations cpu),
                    reg = revertRegs (reg cpu)
                  }
              in trace ("Branch committed, new cpu: " ++ show flushedCPU) flushedCPU
            _ -> cpu { rob = rest, robOffset = robOffset cpu + 1 }
        INSTR_BNE _ _ _ ->
          case robValue entry of
            Just newPc ->  -- Branch taken: flush ROB, reset stations, update PC
              let flushedCPU = cpu {
                    pc = newPc,
                    rob = [],
                    robOffset = robOffset cpu + length (rob cpu),
                    stations = map clearStation (stations cpu),
                    reg = revertRegs (reg cpu)
                  }
              in trace ("Branch committed, new cpu: " ++ show flushedCPU) flushedCPU
            _ -> cpu { rob = rest, robOffset = robOffset cpu + 1 }
        _ -> cpu { rob = rest, robOffset = robOffset cpu + 1 }
      else cpu
  where
    unwrap (Just x) = x
    unwrap Nothing  = error "ROB entry not ready"
    clearStation s = s { busy = False }

freeStoreStation :: [ResStation] -> Int -> [ResStation]
freeStoreStation stations' absTag =
  map (\s -> if robTagRS s == Just absTag then s { busy = False } else s) stations'

initResStation :: ResStation
initResStation = ResStation { rsid = RS_ADD1, op = OP_ADD, vj = 0, vk = 0, qj = Nothing, qk = Nothing, a = 0, busy = False, robTagRS = Nothing }

initCPU :: [Instruction] -> CPU
initCPU prog = CPU {
    reg = mvMultiplicationRegs,
    pc = 0,
    mem = mvMultiplicationMem,
    stations = [initResStation { rsid = RS_ADD1 }
               ,initResStation { rsid = RS_ADD2 }
               ,initResStation { rsid = RS_LS1 }],
    rob = [],
    robOffset = 0,
    program = prog
  }
  
revertRegs :: [RegValue] -> [RegValue]
revertRegs regs =
  map (\x -> case x of
    Pending _ oldVal -> Val oldVal
    Val v -> Val v) regs
  
updateRegs :: [RegValue] -> RegID -> Int -> [RegValue]
updateRegs regs r value =
  let oldVal = (regs !! (fromEnum r))
      idx = fromEnum r
      newRegValue = case oldVal of 
        Pending _ x -> Pending value x
        Val x -> Pending value x
      newRegs = writeToSeq regs idx newRegValue
  in newRegs

mapRegIDToValue :: CPU -> RegID -> RegValue
mapRegIDToValue cpu r = (reg cpu) !! fromEnum r

writeToSeq :: [a] -> Int -> a -> [a]
writeToSeq xs i x = take i xs ++ [x] ++ drop (i + 1) xs

executeInstructions :: CPU -> IO CPU
executeInstructions cpu
  | pc cpu >= length (program cpu) =
      if all (not . busy) (stations cpu) && null (rob cpu)
      then return cpu
      else do
        let afterExec = runReservationStations cpu
        let afterCommit = commitROB afterExec
        executeInstructions afterCommit
  | otherwise = do
      let instr = program cpu !! pc cpu
      newCpu <- case instr of
        INSTR_NOP -> return (Just cpu { pc = pc cpu + 1 })
        INSTR_MOV r1 r2 ->
          let newCpu = cpu { reg = writeToSeq (reg cpu) (fromEnum r1) (mapRegIDToValue cpu r2) }
          in return (Just newCpu { pc = pc cpu + 1 })
        _ -> issueInstruction cpu instr
      case newCpu of
        Nothing -> do
          let afterExec = runReservationStations cpu
          let afterCommit = commitROB afterExec
          executeInstructions afterCommit
        Just cpu' -> executeInstructions cpu' { pc = pc cpu' + 1 }

main :: IO ()
main = do
    putStrLn "\nExecuting Branch Program:"
    finalCpuState <- executeInstructions (initCPU mvMultiplication)
    putStrLn "\nFinal CPU State after Branch Program:"
    print finalCpuState
