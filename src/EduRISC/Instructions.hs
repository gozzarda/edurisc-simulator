module EduRISC.Instructions where

import Data.Int
import Data.Word

import EduRISC.Registers

data Inst = ADD Reg Reg Reg
          | SUB Reg Reg Reg
          | AND Reg Reg Reg
          | OR Reg Reg Reg
          | NAND Reg Reg Reg
          | XOR Reg Reg Reg
          | SLL Reg Reg Reg
          | SRA Reg Reg Reg
          | MOVLI Reg Word8
          | MOVUI Reg Word8
          | JRN Reg Int8
          | JRZ Reg Int8
          | JRP Reg Int8
          -- | Reserved
          | LOD Reg Reg
          | STR Reg Reg
          deriving (Ord, Eq, Show)