
module ALU where

import Types

-- CLaSH-provided hardware stuff
import CLaSH.Sized.Unsigned (Unsigned)

data WithCarry = WithCarry | WithoutCarry

type Reg = Unsigned 5
type TopReg = Unsigned 4
type MidReg = Unsigned 3
type RegPair = Unsigned 4


type SmallConstant = Unsigned 6
type Constant = Unsigned 8

data WideReg = W | X | Y | Z 
data UseWide = UseY | UseZ
data Direction = Load | Store 

data Instruction =
    NOP
    | MOVW RegPair RegPair
    | MULS TopReg TopReg 
    | MULSU MidReg MidReg 
    | FMUL MidReg MidReg
    | FMULSU MidReg MidReg
    | CP Reg Reg WithCarry
    | SB Reg Reg WithCarry
    | ADD Reg Reg WithCarry --also handles rotate left
    | CPSE Reg Reg
    | AND Reg Reg
    | EOR Reg Reg
    | OR Reg Reg
    | MOV Reg Reg
    | CPI TopReg Constant
    | SUBI TopReg Constant WithCarry 
    | ORI TopReg Constant 
    | ANDI TopReg Constant 
    | TNS Reg SmallConstant Direction

