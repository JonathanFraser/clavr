{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module AVR.ISA
    ( module AVR.ISA.Types
    , module AVR.ISA.Arith
    , module AVR.ISA.Branch
    , module AVR.ISA.BitOps
    , module AVR.ISA.Mem
    , avrCoreInstrs
    , avrMulInstrs
    , avrExtInstrs
    , avrIrqBody
    , avrISAWith
    , avrATtinyISA
    , avrATmegaISA
    , avrATxmegaISA
    , avrChip
    ) where

import Prelude hiding (Word)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)

import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce, slice, add, mul, shiftL, shiftR, xor, (.&.), (.|.))
import Hdl.Types (HdlType)
import Isacle.ISA
import AVR.ISA.Types
import AVR.ISA.Arith
import AVR.ISA.Branch
import AVR.ISA.BitOps
import AVR.ISA.Mem

-- ---------------------------------------------------------------------------
-- Instruction groups
-- ---------------------------------------------------------------------------

-- | 16-bit instructions present on all AVR devices.
avrCoreInstrs :: AVR m pcW => [m ()]
avrCoreInstrs =
    [ instrNOP
    , instrMOVW
    , instrADD, instrADC, instrSUB, instrSBC
    , instrAND, instrOR,  instrEOR, instrMOV
    , instrCP,  instrCPC, instrCPSE
    , instrLDI, instrSUBI, instrSBCI, instrANDI, instrORI, instrCPI
    , instrINC, instrDEC, instrCOM, instrNEG, instrASR, instrLSR, instrROR, instrSWAP
    , instrADIW, instrSBIW
    , instrPUSH, instrPOP
    , instrRJMP, instrRCALL, instrRET, instrRETI
    , instrBRBS, instrBRBC
    , instrBSET, instrBCLR, instrBST, instrBLD
    , instrSBRC, instrSBRS
    , instrCBI,  instrSBI, instrSBIC, instrSBIS
    , instrIN,   instrOUT
    , instrLD_Z,  instrLD_Zplus,  instrLD_Zminus
    , instrLD_Y,  instrLD_Yplus,  instrLD_Yminus
    , instrLD_X,  instrLD_Xplus,  instrLD_Xminus
    , instrST_Z,  instrST_Zplus,  instrST_Zminus
    , instrST_Y,  instrST_Yplus,  instrST_Yminus
    , instrST_X,  instrST_Xplus,  instrST_Xminus
    , instrIJMP, instrICALL
    ]

-- | Hardware multiply instructions (absent on most ATtiny devices).
avrMulInstrs :: AVR m pcW => [m ()]
avrMulInstrs = [ instrMUL, instrMULS ]

-- | 32-bit (two-word) instructions — JMP, CALL, LDS, STS.
avrExtInstrs :: AVR m pcW => [m ()]
avrExtInstrs = [ instrJMP, instrCALL, instrLDS, instrSTS ]

-- ---------------------------------------------------------------------------
-- Interrupt body
-- ---------------------------------------------------------------------------

-- | AVR interrupt service entry sequence.
-- Saves PC bytes to the stack, clears the I flag, then jumps to the
-- externally-supplied vector address.  A third byte is pushed for 22-bit PCs.
avrIrqBody :: forall m pcW. (AVR m pcW, MonadIRQ m, HdlType (IrqAddr m)) => m ()
avrIrqBody = do
    irqGate (readFlag avrFlagI)
    spR <- cpu avrSP
    pcR <- cpu avrPC
    pc  <- readReg pcR
    sp  <- readReg spR
    one <- litC 1
    eight <- litC 8
    -- AVR stack grows DOWN: push PC byte @k@ (low-first) at @SP-k@, then SP -= n.
    -- The generic 'push' can't be used twice here — it re-reads SP, which is not
    -- seen updated within one body, so both writes would collide.  Compute the
    -- addresses explicitly and write SP once (as LCALL-style pushes do), so RETI
    -- can recover the interrupted PC.
    lo    <- resizeBits pc
    writeMem sp lo                       -- PCL at SP
    hi    <- resizeBits (shiftR pc eight)
    let sp1 = sp - one
    writeMem sp1 hi                      -- PCH at SP-1
    two   <- litC 2
    finalSp <- if fromIntegral (natVal (Proxy @pcW)) > (16 :: Int)
                 then do
                   sixteen <- litC 16
                   tb  <- resizeBits (shiftR pc sixteen)
                   three <- litC 3
                   writeMem (sp1 - one) tb              -- top byte at SP-2
                   pure (sp - three)
                 else pure (sp - two)
    writeReg spR finalSp
    writeFlag avrFlagI 0
    vec    <- irqVector
    vecPcW <- resizeBits vec
    writeReg pcR vecPcW

-- ---------------------------------------------------------------------------
-- ISADef builder and per-variant ISA definitions
-- ---------------------------------------------------------------------------

-- | Assemble an ISADef from a caller-supplied instruction list.
avrISAWith :: (AVR m pcW, MonadIRQ m, HdlType (IrqAddr m)) => [m ()] -> ISADef m
avrISAWith instrs = defineISA ISADef
    { isaPc            = SomeCPURegister <$> cpu avrPC
    , isaInterruptBody = Just avrIrqBody
    , isaReset         = do
        resetReg  avrPC 0x0000
        resetReg  avrSP 0x09FF  -- top of 2 KB SRAM (0x0200–0x09FF)
        resetFlag avrFlagC Lo
        resetFlag avrFlagZ Lo
        resetFlag avrFlagN Lo
        resetFlag avrFlagV Lo
        resetFlag avrFlagS Lo
        resetFlag avrFlagH Lo
        resetFlag avrFlagT Lo
        resetFlag avrFlagI Lo
    , isaInstrs       = instrs
    }

-- | ATtiny — 16-bit PC, core instructions only (no multiply, no 32-bit ops).
avrATtinyISA :: (AVR m 16, MonadIRQ m, HdlType (IrqAddr m)) => ISADef m
avrATtinyISA = avrISAWith avrCoreInstrs

-- | ATmega — 16-bit PC, full instruction set including multiply and 32-bit ops.
avrATmegaISA :: (AVR m 16, MonadIRQ m, HdlType (IrqAddr m)) => ISADef m
avrATmegaISA = avrISAWith (avrCoreInstrs ++ avrMulInstrs ++ avrExtInstrs)

-- | ATxmega / large AVR — 22-bit PC, full instruction set.
avrATxmegaISA :: (AVR m 22, MonadIRQ m, HdlType (IrqAddr m)) => ISADef m
avrATxmegaISA = avrISAWith (avrCoreInstrs ++ avrMulInstrs ++ avrExtInstrs)

-- | The ATmega \"chip\": storage core ('avrCPUDef') + ISA ('avrATmegaISA')
-- bundled, with the widths pinned (8-bit word, 16-bit data address, 16-bit code
-- words and code address).  Hand this to 'createHarvardCPU' — no @\@core \@width@
-- type applications needed.
avrChip :: Chip (AvrCore 16) (AVRALU 16) 8 16 16 16
avrChip = Chip avrCPUDef avrATmegaISA
