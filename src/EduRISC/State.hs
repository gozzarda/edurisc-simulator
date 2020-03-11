module EduRISC.State where

import Data.Int
import Data.Map.Lazy (Map)
import Data.Map.Lazy as Map
import Data.Word
import Numeric (showHex)

import EduRISC.Registers

type RegMap = Map Reg Word16
type MemMap = Map Word16 Word16
data CpuState = CpuState { state_regs :: RegMap, state_mem :: MemMap }

instance Show CpuState where
  show = (Map.foldlWithKey f "") . state_regs
    where
      f s r v = s ++ showReg r v
      showReg r v = show r ++ ":" ++ showHex v "\t"

initState :: Word16 -> [Word16] -> CpuState
initState bootAddress memImage = CpuState { state_regs = initReg bootAddress, state_mem = initMem memImage}

nullReg :: RegMap
nullReg = Map.fromAscList $ zip (enumFrom R0) (repeat 0)

initReg :: Word16 -> RegMap
initReg v = Map.insert RF v nullReg

initMem :: [Word16] -> MemMap
initMem = Map.fromAscList . (zip [0..])