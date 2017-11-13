
module AVR where
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import CLaSH.Sized.Vector 
import CLaSH.Sized.Unsigned
import CLaSH.Prelude
import Core 

type Reg = Unsigned 5
type TopReg = Unsigned 4
type MidReg = Unsigned 3
type RegPair = Unsigned 4

type UCWord = Unsigned 8

type SmallConstant = Unsigned 6
type Constant = UCWord

data WideReg = W | X | Y | Z 

data ExpandReg = RAMPX | RAMPY | RAMPZ 

data Instruction =
    NOP
    | MOV Reg Reg
    | LDI TopReg UCWord


data CoreData = CoreData {
    registers :: Vec 32 Word
    ,pc :: Unsigned 22 
}

--class ALU state where 
--    readStep :: instr -> state -> readinstr
--    processStep :: instr -> state -> readval -> state
--    writeStep :: instr -> state -> writeinstr 
--    moveStep :: instr -> state -> codeloc
readmem _ _ = Nothing 
process _ state _ = state 
writemem _ _ = Nothing
loc _ _ = 0

instance ALU CoreData where
    readStep = readmem 
    processStep = process 
    writeStep = writemem 
    moveStep = loc