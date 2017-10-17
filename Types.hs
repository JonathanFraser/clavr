{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- The name of our module
module Types where
    
-- CLaSH-provided hardware stuff
import CLaSH.Sized.Unsigned (Unsigned)

type Reg = Unsigned 5
type TopReg = Unsigned 4
type MidReg = Unsigned 3
type RegPair = Unsigned 4


type SmallConstant = Unsigned 6
type Constant = Unsigned 8

data WideReg = W | X | Y | Z 

data ExpandReg = RAMPX | RAMPY | RAMPZ 
