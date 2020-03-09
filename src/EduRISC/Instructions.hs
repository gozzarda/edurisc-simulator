module EduRISC.Instructions where

import Data.Int
import Data.Word

import EduRISC.Registers

data MemOp = LOAD | STORE deriving (Ord, Eq, Show)
data Inst = ADD Reg Reg Reg
          | SUB Reg Reg Reg
          | MUL Reg Reg Reg
          | AND Reg Reg Reg
          | OR Reg Reg Reg
          | NAND Reg Reg Reg
          | XOR Reg Reg Reg
          | SLL Reg Reg Reg
          | SRL Reg Reg Reg
          | SRA Reg Reg Reg
          | MOVLI Reg Word8
          | MOVUI Reg Word8
          | JRN Reg Int8
          | JRZ Reg Int8
          | JRP Reg Int8
          | MEM MemOp Reg Reg
          deriving (Ord, Eq, Show)