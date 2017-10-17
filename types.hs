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


data Register = Register (Unsigned 5)

data ExpandReg = RAMPX | RAMPY | RAMPZ 

data Word = Word (Unsigned 8)
