module EduRISC.Instructions.Encode
  ( encode
  ) where

import Data.Bits
import Data.Int
import Data.Map.Lazy (Map)
import Data.Map.Lazy as Map
import Data.Word

import EduRISC.Instructions
import EduRISC.Registers

encode :: Inst -> Word16
encode (ADD d l r) = encodeRRR 0x0 d l r
encode (SUB d l r) = encodeRRR 0x1 d l r
encode (MUL d l r) = encodeRRR 0x2 d l r
encode (AND d l r) = encodeRRR 0x3 d l r
encode (OR d l r) = encodeRRR 0x4 d l r
encode (NAND d l r) = encodeRRR 0x5 d l r
encode (XOR d l r) = encodeRRR 0x6 d l r
encode (SLL d l r) = encodeRRR 0x7 d l r
encode (SRL d l r) = encodeRRR 0x8 d l r
encode (SRA d l r) = encodeRRR 0x9 d l r
encode (MOVLI d v) = encodeRW 0xA d v
encode (MOVUI d v) = encodeRW 0xB d v
encode (JRN r o) = encodeRI 0xC r o
encode (JRZ r o) = encodeRI 0xD r o
encode (JRP r o) = encodeRI 0xE r o
encode (MEM LOAD m r) = encodeBRR 0xF 0x0 m r
encode (MEM STORE m r) = encodeBRR 0xF 0x8 m r

encodeR :: Reg -> Word16
encodeR = fromIntegral . fromEnum

encodeRRR :: Word16 -> Reg -> Reg -> Reg -> Word16
encodeRRR c d l r = (shift c 12) .|. (shift (encodeR d) 8) .|. (shift (encodeR l) 4) .|. (encodeR r)

encodeRW :: Word16 -> Reg -> Word8 -> Word16
encodeRW c d v = (shift c 12) .|. (shift (encodeR d) 8) .|. (fromIntegral v :: Word16)

encodeRI :: Word16 -> Reg -> Int8 -> Word16
encodeRI c d v = (shift c 12) .|. (shift (encodeR d) 8) .|. (fromIntegral v :: Word16) .&. 0xFF

encodeBRR :: Word16 -> Word16 -> Reg -> Reg -> Word16
encodeBRR c b m r = (shift c 12) .|. (shift b 8) .|. (shift (encodeR m) 4) .|. (encodeR r)