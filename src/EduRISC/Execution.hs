module EduRISC.Execution where

import Control.Monad.State
import Data.Bits
import Data.Int
import Data.Map.Lazy (Map)
import Data.Map.Lazy as Map
import Data.Word

import EduRISC.Instructions
import EduRISC.Instructions.Decode
import EduRISC.Registers
import EduRISC.State

data CpuSignal = SigNormal | SigHalt deriving (Show, Eq)
type EvalCpu a = State CpuState a

runImg :: [Word16] -> CpuState
runImg img = execState run (initState 0 img)

run :: EvalCpu ()
run = loop
  where
    loop = do
      sig <- step
      when (sig == SigNormal) loop
      return ()

step :: EvalCpu CpuSignal
step = do
  inst <- fetchInst
  incPC
  doInst inst

fetchInst :: EvalCpu Inst
fetchInst = fmap decode fetchOpcode

fetchOpcode :: EvalCpu Word16
fetchOpcode = getPC >>= getMem

doInst :: Inst -> EvalCpu CpuSignal
doInst (ADD d l r) = doBinOp (+) d l r
doInst (SUB d l r) = doBinOp (-) d l r
doInst (MUL d l r) = doBinOp (*) d l r
doInst (AND d l r) = doBinOp (.&.) d l r
doInst (OR d l r) = doBinOp (.|.) d l r
doInst (NAND d l r) = doBinOp (nand) d l r
doInst (XOR d l r) = doBinOp (xor) d l r
doInst (SLL d l r) = doBinOp (shiftLLogic) d l r
doInst (SRL d l r) = doBinOp (shiftRLogic) d l r
doInst (SRA d l r) = doBinOp (shiftRArith) d l r
doInst (MOVLI d v) = movli d v
doInst (MOVUI d v) = movui d v
doInst (JRN r o) = jrn r o
doInst (JRZ R0 0xFF) = return SigHalt
doInst (JRZ r o) = jrz r o
doInst (JRP r o) = jrp r o
doInst (MEM LOAD m r) = load m r
doInst (MEM STORE m r) = store m r

getReg :: Reg -> EvalCpu Word16
getReg reg = do
  cpu <- get
  let regs = state_regs cpu
  return $ Map.findWithDefault 0 reg regs

setReg :: Reg -> Word16 -> EvalCpu ()
setReg R0 _ = return ()
setReg reg val = do
  cpu <- get
  let regs = state_regs cpu
  let regs' = Map.insert reg val regs
  put cpu { state_regs = regs' }

getPC :: EvalCpu Word16
getPC = getReg RF

setPC :: Word16 -> EvalCpu ()
setPC = setReg RF

incPC :: EvalCpu ()
incPC = getPC >>= setPC . (+1)

getMem :: Word16 -> EvalCpu Word16
getMem ind = do
  cpu <- get
  let mem = state_mem cpu
  return $ Map.findWithDefault 0 ind mem

setMem :: Word16 -> Word16 -> EvalCpu ()
setMem ind val = do
  cpu <- get
  let mem = state_mem cpu
  let mem' = Map.insert ind val mem
  put cpu { state_mem = mem' }

doBinOp :: (Word16 -> Word16 -> Word16) -> Reg -> Reg -> Reg -> EvalCpu CpuSignal
doBinOp f d l r = do
  lv <- getReg l
  rv <- getReg r
  let result = f lv rv
  setReg d result
  return SigNormal

nand :: Word16 -> Word16 -> Word16
nand l r = complement (l .&. r)

shiftLLogic :: Word16 -> Word16 -> Word16
shiftLLogic v o = shiftL v $ fromIntegral (asSigned o)

shiftRLogic :: Word16 -> Word16 -> Word16
shiftRLogic v o = shiftR v $ fromIntegral (asSigned o)

shiftRArith :: Word16 -> Word16 -> Word16
shiftRArith v o = fromIntegral $ shiftR (fromIntegral v :: Int16) $ fromIntegral (asSigned o)

sext :: Word8 -> Word16
sext v = if s then u .|. v' else v'
  where
    v' = fromIntegral v :: Word16
    u = shift (complement zeroBits) 8
    s = (countLeadingZeros v) == 0

movli :: Reg -> Word8 -> EvalCpu CpuSignal
movli d v = do
  setReg d $ sext v
  return SigNormal

movui :: Reg -> Word8 -> EvalCpu CpuSignal
movui d v = do
  cur <- getReg d
  let lwr = cur .&. 0xFF
  let upr = shift (fromIntegral v :: Word16) 8
  let res = upr .|. lwr
  setReg d res
  return SigNormal

jrc :: (Word16 -> Bool) -> Reg -> Int8 -> EvalCpu CpuSignal
jrc f r o = do
  val <- getReg r
  cur <- getPC
  let res = cur + (fromIntegral o)
  when (f val) $ setPC res
  return SigNormal

jrn :: Reg -> Int8 -> EvalCpu CpuSignal
jrn = jrc ((<0).asSigned)

jrz :: Reg -> Int8 -> EvalCpu CpuSignal
jrz = jrc (==0)

jrp :: Reg -> Int8 -> EvalCpu CpuSignal
jrp = jrc ((>0).asSigned)

asSigned :: Word16 -> Int16
asSigned = fromIntegral

load :: Reg -> Reg -> EvalCpu CpuSignal
load m r = do
  ind <- getReg m
  val <- getMem ind
  setReg r val
  return SigNormal

store :: Reg -> Reg -> EvalCpu CpuSignal
store m r = do
  ind <- getReg m
  val <- getReg r
  setMem ind val
  return SigNormal