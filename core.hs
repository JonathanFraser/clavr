{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- The name of our module
module CORE where
    
-- CLaSH-provided hardware stuff
import CLaSH.Sized.Unsigned (Unsigned)
import CLaSH.Sized.Vector (Vec((:>), Nil), 
        (!!), replace, repeat, (++))
import CLaSH.Class.Resize (zeroExtend)
import CLaSH.Sized.BitVector (BitVector, (++#), Bit)
import CLaSH.Class.BitPack (pack, unpack)
import CLaSH.Prelude (slice)
import CLaSH.Promoted.Nat.Literals as Nat
import CLaSH.Signal (Signal, register, sample)


data BaseRegister = 
        R0 
        | R1 
        | R2 
        | R3 
        | R4
        | R5 
        | R6 
        | R7 deriving Show

data ExpandReg = 
        RAMPX
        | RAMPY
        | RAMPZ deriving Show

data Word = Word (Unsigned 8)