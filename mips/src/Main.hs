module Main (main) where
import Debug.Trace

-- subset of MIPS instructions, (add, mov, lw, sw)

data Instruction = 
    INSTR_ADD RegID RegID RegID |
    INSTR_MOV RegID RegID |
    INSTR_LW RegID Int |
    INSTR_SW RegID Int |
    INSTR_NOP
  deriving (Show, Eq)
  
data Operation = OP_ADD | OP_LW | OP_SW | OP_MV deriving (Show, Eq, Enum)
  
data RegID = 
    R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
  deriving (Show, Eq, Enum)
  
data RegValue = 
    Val Int | Pending RSID
  deriving (Show, Eq)
  
data RSID = RS_ADD1 | RS_ADD2 | RS_LS1 deriving (Show, Eq, Enum)
  
data ResStation = ResStation {
    rsid :: RSID,
    op :: Operation,
    vj :: Int,
    vk :: Int,
    qj :: Maybe RSID,
    qk :: Maybe RSID,
    a :: Int,
    busy :: Bool
  } deriving (Show, Eq)
  
sampleInstructions :: [Instruction]
sampleInstructions = 
  [ INSTR_ADD R1 R1 R2
  , INSTR_ADD R2 R3 R4
  , INSTR_MOV R2 R4
  , INSTR_LW R3 4
 -- , INSTR_SW R4 4
  , INSTR_NOP
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
  
issueInstruction :: CPU -> Instruction -> IO (Maybe CPU)
issueInstruction cpu instr = do
  putStrLn ("Issuing instruction " ++ show instr)
  newCpu <- (case chooseStation cpu instr of
    Just stationID -> do
      let newStation = case instr of
            INSTR_ADD rd r1 r2 -> createAddStation cpu r1 r2 stationID
            INSTR_LW r1 addr    -> createLoadStation cpu instr stationID
            INSTR_SW r1 addr    -> createLoadStation cpu instr stationID
            _                   -> error "Invalid instruction for reservation station"
      let newStations = writeToSeq (stations cpu) (fromEnum stationID) newStation
      let newCpu = case instr of
            INSTR_ADD rd _ _ -> cpu { stations = newStations, reg = writeToSeq (reg cpu) (fromEnum rd) (Pending stationID) }
            INSTR_LW rd _    -> cpu { stations = newStations, reg = writeToSeq (reg cpu) (fromEnum rd) (Pending stationID) }
            _                -> cpu { stations = newStations }
      putStrLn ("Issued to station " ++ show stationID)
      return (Just newCpu)
    Nothing -> do
      putStrLn "No available reservation stations for operation"
      return Nothing)
  return newCpu


createAddStation :: CPU -> RegID -> RegID -> RSID -> ResStation
createAddStation cpu r1 r2 rsid = ResStation {
  rsid = rsid,
  op = OP_ADD,
  vj = case state1 of
    Val v -> v
    Pending _ -> 0,
  vk = case state2 of
    Val v -> v
    Pending _ -> 0,
  qj = case state1 of
    Val _ -> Nothing
    Pending rsid -> Just rsid,
  qk = case state2 of
    Val _ -> Nothing
    Pending rsid -> Just rsid,
  a = 0,
  busy = True
} where 
  state1 = mapRegIDToValue cpu r1
  state2 = mapRegIDToValue cpu r2
  
createLoadStation :: CPU -> Instruction -> RSID -> ResStation
createLoadStation cpu (INSTR_LW r1 addr) rsid = ResStation {
  rsid = rsid,
  op = OP_LW,
  qj = case state1 of
    Val _ -> Nothing
    Pending rsid -> Just rsid,
  qk = Nothing,
  vj = case state1 of
    Val v -> v
    Pending _ -> 0,
  vk = 0,
  a = addr,
  busy = True
} where state1 = mapRegIDToValue cpu r1
createLoadStation cpu (INSTR_SW r1 addr) rsid = ResStation {
  rsid = rsid,
  op = OP_SW,
  qj = case state1 of
    Val _ -> Nothing
    Pending rsid -> Just rsid,
  qk = case state1 of
    Val _ -> Nothing
    Pending rsid -> Just rsid,
  vj = case state1 of
    Val v -> v
    Pending _ -> 0,
  vk = case state1 of
    Val v -> v
    Pending _ -> 0,
  a = addr,
  busy = True
} where state1 = mapRegIDToValue cpu r1

-- A simple function to execute a list of instructions
executeInstructions :: CPU -> [Instruction] -> IO CPU
executeInstructions cpu [] = case all (not . busy) (stations cpu) of
  True -> return cpu -- all stations are free, we can return the CPU state
  False -> do
    let newCpu = runReservationStations cpu
    executeInstructions newCpu [] -- run the reservation stations until they are all free
executeInstructions cpu (instr:instrs) = do
  newCpu <- (case instr of
    INSTR_NOP -> return (Just cpu)
    INSTR_MOV r1 r2 -> do
      let newCpu = cpu { reg = writeToSeq (reg cpu) (fromEnum r1) (mapRegIDToValue cpu r2) }
      return (Just newCpu)
    _ -> do
      newCpu <- issueInstruction cpu instr 
      return newCpu)
  case newCpu of
    Nothing -> do
      (executeInstructions (runReservationStations cpu) (instr:instrs)) -- nothing to issue, just run the reservation stations
    Just newCpu' -> (executeInstructions newCpu' instrs)-- not everything issued, we pretend like nothing runs until everything is issued
    
-- A function to run the reservation stations
runReservationStations :: CPU -> CPU
runReservationStations cpu = trace "attempting to run stations" (do
  let oldStations = stations cpu
  let newCpu = foldl executeAndWrite cpu oldStations
  newCpu)
  
executeAndWrite :: CPU -> ResStation -> CPU
executeAndWrite cpu station = 
  if busy station 
  then case op station of
    OP_ADD -> trace "attempting add" (executeAdd cpu station{busy = False})
    OP_LW -> trace "attempting load" (executeLoad cpu station{busy = False}) 
    OP_SW -> trace "attempting store" (executeStore cpu station{busy = False}) 
    _ -> cpu
  else cpu
  
executeAdd :: CPU -> ResStation -> CPU
executeAdd cpu station = 
  if qj station == Nothing && qk station == Nothing
  then trace "add ready for execution" cpu { reg = overwritePendingRegs (reg cpu) (rsid station) (vj station + vk station), 
             stations = overwritePendingResStations (stations cpu) (rsid station) (vj station + vk station) }
  else cpu
  
executeLoad :: CPU -> ResStation -> CPU
executeLoad cpu station = 
  if qj station == Nothing
  then trace "load ready for execution" cpu { reg = overwritePendingRegs (reg cpu) (rsid station) (mem cpu !! (vj station + a station)), 
             stations = overwritePendingResStations (stations cpu) (rsid station) (mem cpu !! (vj station + a station)) }
  else cpu
  
executeStore :: CPU -> ResStation -> CPU
executeStore cpu station = 
  if qj station == Nothing
  then do 
    trace "load ready for execution" cpu { mem = writeToSeq (mem cpu) (vj station + a station) (vk station) }
  else cpu
    

overwritePendingRegs :: [RegValue] -> RSID -> Int -> [RegValue]
overwritePendingRegs regs rsid result = 
  map (\x -> case x of
    Pending rsid -> if rsid == rsid then Val result else x
    _ -> x) regs
    
overwritePendingResStations :: [ResStation] -> RSID -> Int -> [ResStation]
overwritePendingResStations stations rsid result = 
  map (\x -> do 
    let x' = if qj x == Just rsid then x { vj = result, qj = Nothing } else x
    let x'' = if qk x' == Just rsid then x' { vk = result, qk = Nothing } else x'
    x''
  ) stations
  
data CPU = 
    CPU { reg :: [RegValue], 
          stations :: [ResStation],
          pc :: Int, 
          mem :: [Int] }
  deriving (Show) 
  
-- Initialize the reservation stations
initResStation :: ResStation
initResStation = ResStation { rsid = RS_ADD1, op = OP_ADD, vj = 0, vk = 0, qj = Nothing, qk = Nothing, a = 0, busy = False }
  
-- Initialize the CPU with some registers and memory
initCPU :: CPU
initCPU = CPU { reg = [Val 2, Val 3, Val 4, Val 5, Val 6, Val 7, Val 8, Val 9], pc = 0, mem = replicate 64 0, stations = [initResStation {rsid = RS_ADD1}, initResStation {rsid = RS_ADD2}, initResStation {rsid = RS_LS1}] }

-- map registers to their values
mapRegIDToValue :: CPU -> RegID -> RegValue
mapRegIDToValue cpu r = (reg cpu) !! fromEnum r

writeToSeq :: [a] -> Int -> a -> [a]
writeToSeq xs i x = take i xs ++ [x] ++ drop (i + 1) xs
 
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
