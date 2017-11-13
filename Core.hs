module Core where
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


import CLaSH.Sized.Vector 
import CLaSH.Sized.Unsigned
import CLaSH.Prelude

import CLaSH.Sized.Vector (Vec((:>), Nil), 
        (!!), replace, repeat, (++))
--import CLaSH.Class.Resize (zeroExtend)
--import CLaSH.Sized.BitVector (BitVector, (++#), Bit)
--import CLaSH.Class.BitPack (pack, unpack)
--import CLaSH.Prelude (slice)
--import CLaSH.Promoted.Nat.Literals as Nat
--import CLaSH.Signal (Signal, register, sample)

class ALU state where 
    readStep :: instr -> state -> Maybe readinstr
    processStep :: instr -> state -> Maybe readval -> state
    writeStep :: instr -> state -> Maybe writeinstr 
    moveStep :: instr -> state -> codeloc

class CodeMem code where 
        load :: codelocation -> code -> instruction

class RAM ram where 
        readRAM :: readinstr -> ram -> readval
        writeRAM :: writeinstr -> ram -> ram

data SimpleStates = Fetch | Read | Process | Write

simple :: ALU a => a -> instr -> init -> Signal (Maybe readval,Maybe instr) -> Signal (Maybe readinstr,Maybe writeinstr,Maybe codelocation)
simple cpu nopvalue init inputs = signal (Nothing,Nothing,Nothing)
