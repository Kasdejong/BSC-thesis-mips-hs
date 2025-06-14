module Main (main) where
import Debug.Trace

-- Subset of MIPS instructions (add, mov, lw, sw)
data Instruction =
    INSTR_ADD RegID RegID RegID
  | INSTR_MOV RegID RegID
  | INSTR_LW RegID Int
  | INSTR_SW RegID Int
  | INSTR_NOP
  deriving (Show, Eq)

data Operation = OP_ADD | OP_LW | OP_SW | OP_MV deriving (Show, Eq, Enum)

data RegID =
    R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
  deriving (Show, Eq, Enum)

data RegValue =
    Val Int | Pending Int
  deriving (Show, Eq)

data RSID = RS_ADD1 | RS_ADD2 | RS_LS1 deriving (Show, Eq, Enum)

data ROBEntry = ROBEntry {
    robTag  :: Int,            -- Absolute ROB tag
    robInstr :: Instruction,
    robDest  :: Maybe RegID,   -- Register destination (for stores, can be Nothing)
    robAddr  :: Maybe Int,     -- For stores/loads: memory address
    robValue :: Maybe Int,     -- Value/result (if ready)
    robReady :: Bool           -- Is result ready?
} deriving (Show, Eq)

data ResStation = ResStation {
    rsid   :: RSID,
    op     :: Operation,
    vj     :: Int,
    vk     :: Int,
    qj     :: Maybe Int,    -- Source 1: waiting for ROB tag
    qk     :: Maybe Int,    -- Source 2: waiting for ROB tag
    a      :: Int,
    busy   :: Bool,
    robTagRS :: Maybe Int  -- Which ROB entry does this station write to?
  } deriving (Show, Eq)
  
data CPU = CPU { 
    reg :: [RegValue],
    stations :: [ResStation],
    rob :: [ROBEntry],
    pc :: Int,
    mem :: [Int],
    robOffset :: Int        -- Absolute index of the head of the ROB
  } deriving (Show)

simpleProgram :: [Instruction]
simpleProgram =
  [ INSTR_ADD R1 R1 R2
  , INSTR_ADD R2 R3 R4
  , INSTR_MOV R2 R4
  , INSTR_LW R3 4
  , INSTR_SW R4 4
  , INSTR_NOP
  ]
  
complexProgram :: [Instruction]
complexProgram =
  [ INSTR_SW R1 0
  , INSTR_SW R2 8
  , INSTR_LW R3 0
  , INSTR_LW R4 8
  , INSTR_ADD R5 R3 R4
  , INSTR_SW R5 16
  , INSTR_ADD R3 R5 R4
  , INSTR_SW R3 24
  , INSTR_ADD R6 R5 R3
  , INSTR_ADD R7 R6 R5
  , INSTR_SW R7 32
  , INSTR_LW R0 16
  , INSTR_ADD R1 R0 R0
  , INSTR_SW R1 40
  ]

chooseStation :: CPU -> Instruction -> Maybe RSID
chooseStation cpu (INSTR_ADD _ _ _) =
  if not (busy (stations cpu !! fromEnum RS_ADD1))
  then Just RS_ADD1
  else if not (busy (stations cpu !! fromEnum RS_ADD2))
       then Just RS_ADD2
       else Nothing
chooseStation cpu (INSTR_LW _ _) =
  if not (busy (stations cpu !! fromEnum RS_LS1))
  then Just RS_LS1
  else Nothing
chooseStation cpu (INSTR_SW _ _) =
  if not (busy (stations cpu !! fromEnum RS_LS1))
  then Just RS_LS1
  else Nothing
chooseStation _ _ = error "Invalid instruction for reservation station"

-- Allocate a new ROB entry and return its absolute tag
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
                      INSTR_LW rd _    -> Just rd
                      _                -> Nothing
          (cpuWithROB, absTag) = allocateROBEntry cpu instr destReg
          newStation = case instr of
            INSTR_ADD _ r1 r2 -> createAddStation cpuWithROB r1 r2 stationID (Just absTag)
            INSTR_LW _ _  -> createLoadStation cpuWithROB instr stationID (Just absTag)
            INSTR_SW _ _  -> createStoreStation cpuWithROB instr stationID (Just absTag)
            _                 -> error "Invalid instruction for reservation station"
          newStations = writeToSeq (stations cpuWithROB) (fromEnum stationID) newStation
          newRegs = case destReg of
            Just destRegId -> writeToSeq (reg cpuWithROB) (fromEnum destRegId) (Pending absTag)
            Nothing  -> reg cpuWithROB
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
  vj = case state1 of
    Val v -> v
    Pending _ -> 0,
  vk = case state2 of
    Val v -> v
    Pending _ -> 0,
  qj = case state1 of
    Val _ -> Nothing
    Pending robTag' -> Just robTag',
  qk = case state2 of
    Val _ -> Nothing
    Pending robTag' -> Just robTag',
  a = 0,
  busy = True,
  robTagRS = destRobTag
} where
  state1 = mapRegIDToValue cpu r1
  state2 = mapRegIDToValue cpu r2

createLoadStation :: CPU -> Instruction -> RSID -> Maybe Int -> ResStation
createLoadStation cpu (INSTR_LW r1 addr) selfRsid destRobTag = ResStation {
  rsid = selfRsid,
  op = OP_LW,
  qj = case state1 of
    Val _ -> Nothing
    Pending robTag' -> Just robTag',
  qk = Nothing,
  vj = case state1 of
    Val v -> v
    Pending _ -> 0,
  vk = 0,
  a = addr,
  busy = True,
  robTagRS = destRobTag
} where state1 = mapRegIDToValue cpu r1
createLoadStation _ _ _ _ = error "Invalid instruction for load station"

createStoreStation :: CPU -> Instruction -> RSID -> Maybe Int -> ResStation
createStoreStation cpu (INSTR_SW r1 addr) selfRsid destRobTag = ResStation {
  rsid = selfRsid,
  op = OP_SW,
  qj = case state1 of
    Val _ -> Nothing
    Pending robTag' -> Just robTag',
  qk = case state1 of
    Val _ -> Nothing
    Pending robTag' -> Just robTag',
  vj = case state1 of
    Val v -> v
    Pending _ -> 0,
  vk = case state1 of
    Val v -> v
    Pending _ -> 0,
  a = addr,
  busy = True,
  robTagRS = destRobTag
} where state1 = mapRegIDToValue cpu r1
createStoreStation _ _ _ _ = error "Invalid instruction for store station"

executeInstructions :: CPU -> [Instruction] -> IO CPU
executeInstructions cpu [] =
  case all (not . busy) (stations cpu) && null (rob cpu) of
    True -> return cpu
    False -> do
      let afterExec = runReservationStations cpu
      let afterCommit = commitROB afterExec
      executeInstructions afterCommit []
executeInstructions cpu (instr:instrs) = do
  newCpu <- case instr of
    INSTR_NOP -> return (Just cpu)
    INSTR_MOV r1 r2 -> do
      let newCpu = cpu { reg = writeToSeq (reg cpu) (fromEnum r1) (mapRegIDToValue cpu r2) }
      return (Just newCpu)
    _ -> issueInstruction cpu instr
  case newCpu of
    Nothing -> executeInstructions (commitROB (runReservationStations cpu)) (instr:instrs)
    Just newCpu' -> executeInstructions newCpu' instrs

runReservationStations :: CPU -> CPU
runReservationStations cpu =
  foldl (\c stn -> if busy stn then executeAndWrite c stn else c) cpu (stations cpu)

-- All ROB accesses use absolute tags and robOffset
executeAndWrite :: CPU -> ResStation -> CPU
executeAndWrite cpu station
  | not (busy station) = cpu
  | otherwise = case op station of
      OP_ADD -> executeAdd cpu station
      OP_LW  -> executeLoad cpu station
      OP_SW  -> executeStore cpu station
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
  | qj station == Nothing =
      let loadAddr = a station -- add offset later maybe?
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
      let storeAddr = a station -- add offset later maybe?
          storeVal = vk station
          absTag = case robTagRS station of Just t -> t; Nothing -> error "No ROB tag"
          newROB = updateROBEntryStore (rob cpu) absTag storeAddr storeVal True (robOffset cpu)
      in trace ("store ready for execution, ROB tag " ++ show absTag ++ ", pending store: " ++ show storeVal ++ " at address " ++ show storeAddr ++ " full state: " ++ show cpu) cpu { rob = newROB }
  | otherwise = cpu

-- All ROB accesses use absolute tags and robOffset
overwritePendingRegs :: [RegValue] -> Int -> Int -> [RegValue]
overwritePendingRegs regs absTag result =
  map (\x -> case x of
    Pending tag -> if tag == absTag then Val result else x
    _ -> x) regs

overwritePendingResStations :: [ResStation] -> Int -> Int -> [ResStation]
overwritePendingResStations resStations absTag result =
  map (\x ->
    let x'  = if qj x == Just absTag then x { vj = result, qj = Nothing } else x
        x'' = if qk x' == Just absTag then x' { vk = result, qk = Nothing } else x'
    in x''
  ) resStations

-- Update ROB entry with result (for ADD/LOAD)
updateROBEntry :: [ROBEntry] -> Int -> Maybe Int -> Bool -> Int -> [ROBEntry]
updateROBEntry entries absTag val ready robOffset' =
  let idx = absTag - robOffset'
  in if idx < 0 || idx >= length entries
     then error $ "ROB tag " ++ show absTag ++ " out of bounds (offset " ++ show robOffset' ++ ", len " ++ show (length entries) ++ ")"
     else take idx entries ++
          [ (entries !! idx) { robValue = val, robReady = ready } ] ++
          drop (idx + 1) entries

-- Update ROB entry with store address and value
updateROBEntryStore :: [ROBEntry] -> Int -> Int -> Int -> Bool -> Int -> [ROBEntry]
updateROBEntryStore entries absTag addr val ready robOffset' =
  let idx = absTag - robOffset'
  in if idx < 0 || idx >= length entries
     then error $ "ROB tag " ++ show absTag ++ " out of bounds (offset " ++ show robOffset' ++ ", len " ++ show (length entries) ++ ")"
     else take idx entries ++
          [ (entries !! idx) { robAddr = Just addr, robValue = Just val, robReady = ready } ] ++
          drop (idx + 1) entries

-- Commit phase: commit instructions from the head of the ROB if ready
commitROB :: CPU -> CPU
commitROB cpu =
  case rob cpu of
    [] -> cpu
    (entry:rest) ->
      if robReady entry
      then case robInstr entry of
        INSTR_ADD rd _ _ ->
          let newRegs = writeToSeq (reg cpu) (fromEnum rd) (Val (unwrap (robValue entry)))
              newStations = overwritePendingResStations (stations cpu) (robTag entry) (unwrap (robValue entry))
          in cpu { reg = newRegs, stations = newStations, rob = rest, robOffset = robOffset cpu + 1 }
        INSTR_LW rd _ ->
          let newRegs = writeToSeq (reg cpu) (fromEnum rd) (Val (unwrap (robValue entry)))
              newStations = overwritePendingResStations (stations cpu) (robTag entry) (unwrap (robValue entry))
          in cpu { reg = newRegs, stations = newStations, rob = rest, robOffset = robOffset cpu + 1 }
        INSTR_SW _ _ ->
          let addr = unwrap (robAddr entry)
              val  = unwrap (robValue entry)
              newMem = writeToSeq (mem cpu) addr val
              newStations = freeStoreStation (stations cpu) (robTag entry)
          in cpu { mem = newMem, stations = newStations, rob = rest, robOffset = robOffset cpu + 1 }
        _ -> cpu { rob = rest, robOffset = robOffset cpu + 1 }
      else cpu
  where
    unwrap (Just x) = x
    unwrap Nothing  = error "ROB entry not ready"

-- Helper to free the store station associated with this ROB tag
freeStoreStation :: [ResStation] -> Int -> [ResStation]
freeStoreStation stations' absTag =
  map (\s -> if robTagRS s == Just absTag then s { busy = False } else s) stations'

initResStation :: ResStation
initResStation = ResStation { rsid = RS_ADD1, op = OP_ADD, vj = 0, vk = 0, qj = Nothing, qk = Nothing, a = 0, busy = False, robTagRS = Nothing }

initCPU :: CPU
initCPU = CPU {
    reg = [Val 2, Val 3, Val 4, Val 5, Val 6, Val 7, Val 8, Val 9],
    pc = 0,
    mem = replicate 64 0,
    stations = [initResStation { rsid = RS_ADD1 }
               ,initResStation { rsid = RS_ADD2 }
               ,initResStation { rsid = RS_LS1 }],
    rob = [],
    robOffset = 0
  }

mapRegIDToValue :: CPU -> RegID -> RegValue
mapRegIDToValue cpu r = (reg cpu) !! fromEnum r

writeToSeq :: [a] -> Int -> a -> [a]
writeToSeq xs i x = take i xs ++ [x] ++ drop (i + 1) xs

main :: IO ()
main = do
  let cpu = initCPU
  putStrLn "\nExecuting Simple:"
  finalCpuState <- executeInstructions cpu simpleProgram
  putStrLn "\nFinal CPU State:"
  print finalCpuState
  putStrLn "\nExecuting Complex Program:"
  finalCpuState' <- executeInstructions initCPU complexProgram
  putStrLn "\nFinal CPU State after Complex Program:"
  print finalCpuState'
  return ()
