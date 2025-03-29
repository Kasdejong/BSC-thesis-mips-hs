module Instructions where

import Data.Bits
import Data.Int

-- subset of MIPS instructions, (add, mov, lw, sw)

data Instruction = 
    ADD byte byte |
    MOV byte byte |
    LW byte Int |
    SW byte Int |
    NOP
  deriving (Show, Eq)