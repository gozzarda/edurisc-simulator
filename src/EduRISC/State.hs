module EduRISC.State where

import Data.Int
import Data.Map.Lazy (Map)
import Data.Map.Lazy as Map
import Data.Word

import EduRISC.Registers

type RegMap = Map Reg Word16
type MemMap = Map Word16 Word16
data CpuState = CpuState { state_regs :: RegMap, state_mem :: MemMap }

instance Show CpuState where
  show = show . state_regs

initState :: Word16 -> [Word16] -> CpuState
initState bootAddress memImage = CpuState { state_regs = initReg bootAddress, state_mem = initMem memImage}

initReg :: Word16 -> RegMap
initReg = Map.singleton RF

initMem :: [Word16] -> MemMap
initMem = Map.fromAscList . (zip [0..])

dumpCpuState :: CpuState -> String
dumpCpuState s = show $ state_regs s