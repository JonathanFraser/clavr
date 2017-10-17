{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- The name of our module
module Types where
    
-- CLaSH-provided hardware stuff
import CLaSH.Sized.Unsigned (Unsigned)


data Register = Register (Unsigned 5)

data ExpandReg = RAMPX | RAMPY | RAMPZ 

data Word = Word (Unsigned 8)