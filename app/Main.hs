module Main where

import Data.Word

import EduRISC.Execution
import EduRISC.Instructions
import EduRISC.Instructions.Encode
import EduRISC.Registers
import EduRISC.State

prog :: [Inst]
prog =
  [ MOVLI R1 0x01 -- Const 1
  , MOVLI R2 0x10 -- N
  , MOVLI R3 0xFF -- A = -1
  , MOVLI R4 0x01 -- B = 1
  , AND RE RF RF  -- loop = PC
  , ADD R5 R3 R4  -- C = A + B
  , AND R3 R4 R4  -- A = B
  , AND R4 R5 R5  -- B = C
  , SUB R2 R2 R1  -- N = N - 1
  , JRN R2 1      -- if N > 0:
  , AND RF RE RE  --   goto loop
  , JRZ R0 (-1)   -- HALT
  ]

code :: [Word16]
code = map encode prog

main :: IO ()
main = putStrLn $ show $ runImg code
