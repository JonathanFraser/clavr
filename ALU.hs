
module ALU where

import Types

data WithCarry = WithCarry | WithoutCarry


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
    | TND Reg SmallConstant Direction

