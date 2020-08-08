module EduRISC.Instructions.Decode
  ( decode
  ) where

import Data.Bits
import Data.Int
import Data.Maybe (catMaybes)
import Data.Word

import EduRISC.Instructions
import EduRISC.Registers

decode :: Word16 -> Inst
decode w = head $ catMaybes $ map ($w) [decodeRRR, decodeRW, decodeRI, decodeRR]

decodeR :: Word16 -> Reg
decodeR = toEnum . fromIntegral . (`mod` 0x10)

decodeW :: Word16 -> Word8
decodeW = fromIntegral

decodeI :: Word16 -> Int8
decodeI = fromIntegral

decodeRRR :: Word16 -> Maybe Inst
decodeRRR w = case (shiftR w 12) of
  0x0 -> Just $ ADD d l r
  0x1 -> Just $ SUB d l r
  0x2 -> Just $ AND d l r
  0x3 -> Just $ OR d l r
  0x4 -> Just $ NAND d l r
  0x5 -> Just $ XOR d l r
  0x6 -> Just $ SLL d l r
  0x7 -> Just $ SRA d l r
  _ -> Nothing
  where
    d = decodeR $ shiftR w 8
    l = decodeR $ shiftR w 4
    r = decodeR w

decodeRW :: Word16 -> Maybe Inst
decodeRW w = case (shiftR w 12) of
  0x8 -> Just $ MOVLI d v
  0x9 -> Just $ MOVUI d v
  _ -> Nothing
  where
    d = decodeR $ shiftR w 8
    v = decodeW w

decodeRI :: Word16 -> Maybe Inst
decodeRI w = case (shiftR w 12) of
  0xA -> Just $ JRN r o
  0xB -> Just $ JRZ r o
  0xC -> Just $ JRP r o
  _ -> Nothing
  where
    r = decodeR $ shiftR w 8
    o = decodeI w

decodeRR :: Word16 -> Maybe Inst
decodeRR w = case (shiftR w 12) of
  0xE -> Just $ LOD m r
  0xF -> Just $ STR m r
  _ -> Nothing
  where
    m = decodeR $ shiftR w 4
    r = decodeR w