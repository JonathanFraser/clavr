{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
module AVR.ISA.Types
    ( AVRALU(..)
    , AvrCore(..)
    , twoReg
    , immReg
    , avrCPUDef
    , AVR
    , avrFlagAt
    , setFlagHi, setFlagLo
    , stubArith
    , pcAdvance
    , skipNextIf
    ) where

import Prelude hiding (Word)
import GHC.Generics (Generic, Rep)

import Hdl.Bits hiding ((!!), zeroExtend, signExtend, truncateB, bitCoerce, slice)
import Hdl.Types (HdlType(..), GWidth, genericToBits, genericFromBits)
import Isacle.ISA
import Isacle.ISA.Types (RegisterFile)

-- ---------------------------------------------------------------------------
-- AVR ALU definition record
-- pcW — width of the program counter in bits (16 for ≤32K flash, 22 for larger)
-- ---------------------------------------------------------------------------

data AVRALU pcW = AVRALU
    { avrGPR   :: RegisterFile 32 (Unsigned 8)
    , avrSP    :: CPURegister (Unsigned 16)
    , avrPC    :: CPURegister (Unsigned pcW)
    , avrX     :: CPURegister (Unsigned 16)    -- data pointer (R27:R26)
    , avrY     :: CPURegister (Unsigned 16)    -- data pointer (R29:R28)
    , avrZ     :: CPURegister (Unsigned 16)    -- data pointer (R31:R30)
    , avrSREG  :: CPURegister (Unsigned 8)     -- packed status register
    , avrFlagC :: CPUFlag
    , avrFlagZ :: CPUFlag
    , avrFlagN :: CPUFlag
    , avrFlagV :: CPUFlag
    , avrFlagS :: CPUFlag
    , avrFlagH :: CPUFlag
    , avrFlagT :: CPUFlag
    , avrFlagI :: CPUFlag
    }

-- | The raw ALU /core/: the pure scalar storage state, an 'HdlType' record the
-- synthesiser clocks as one register.  (The register file @GPR@ stays a separate
-- indexed bank — packing it here would regress its indexed writes to a mux tree.)
-- The @AVRALU@ handles above are the lens/alias /view/ the ISA interacts with;
-- each projects into this core or the file (@avrSP@ → @coreSP@, @avrX@ →
-- @GPR[26:27]@, flags → bits of @coreSREG@).
data AvrCore (pcW :: Nat) = AvrCore
    { coreSP   :: Unsigned 16
    , corePC   :: Unsigned pcW
    , coreSREG :: Unsigned 8
    } deriving (Generic)

instance (KnownNat pcW, KnownNat (Width (AvrCore pcW))) => HdlType (AvrCore pcW) where
    type Width (AvrCore pcW) = GWidth (Rep (AvrCore pcW))
    toBits   = genericToBits
    fromBits = genericFromBits

-- | The common two-register AVR encoding shape @\<6 fixed bits\>rd_dddd_rrrr@:
-- both Rd and Rr are 5-bit fields split as (high bit) + (low nibble). Returns
-- @(Rd, Rr)@ placeholders. e.g. ADD is @twoReg "000011"@.
twoReg :: String -> Encoding (Field (Unsigned 5), Field (Unsigned 5))
twoReg pre = do
    fixed pre
    r <- placeholder @(Unsigned 5)
    d <- placeholder @(Unsigned 5)
    bindBits r 1            -- r high  (bit 9)
    bindBits d 1            -- d high  (bit 8)
    bindBits d 4            -- d low   (bits 7-4)
    bindBits r 4            -- r low   (bits 3-0)
    return (d, r)

-- | The upper-register + 8-bit-immediate shape @\<4 fixed bits\>KKKK_dddd_KKKK@:
-- the 8-bit immediate K is split (high nibble, low nibble) and Rd is a 4-bit
-- field (used with offset 16 → R16–R31). Returns @(Rd4, K8)@.
immReg :: String -> Encoding (Field (Unsigned 4), Field (Unsigned 8))
immReg pre = do
    fixed pre
    k <- placeholder @(Unsigned 8)
    bindBits k 4            -- K high nibble (bits 11-8)
    d <- field @(Unsigned 4)  -- dddd (bits 7-4)
    bindBits k 4            -- K low nibble (bits 3-0)
    return (d, k)

-- ---------------------------------------------------------------------------
-- CPUDef — parameterised over PC width via TypeApplications
-- Usage: avrCPUDef @16  or  avrCPUDef @22
-- ---------------------------------------------------------------------------

avrCPUDef :: forall pcW. KnownNat pcW => ISACoreDefinition (AVRALU pcW)
avrCPUDef = do
    endianness LittleEndian
    gpr'  <- newRegFile "GPR"        -- RegisterFile 32 (Unsigned 8)
    sp'   <- reg "SP"   w16
    pc'   <- reg "PC"   (SNat @pcW)
    -- X/Y/Z are not separate storage: they are 16-bit views over GPR pairs
    -- (R27:R26, R29:R28, R31:R30), low byte first — so writing R30/R31 updates Z.
    x'    <- regView "X" gpr' [26, 27]
    y'    <- regView "Y" gpr' [28, 29]
    z'    <- regView "Z" gpr' [30, 31]
    sreg' <- reg "SREG" w8
    -- Flags are bits of SREG (MSB-first: I@7 T@6 H@5 S@4 V@3 N@2 Z@1 C@0).
    i <- newFlag "I" (sreg' ! 7)
    t <- newFlag "T" (sreg' ! 6)
    h <- newFlag "H" (sreg' ! 5)
    s <- newFlag "S" (sreg' ! 4)
    v <- newFlag "V" (sreg' ! 3)
    n <- newFlag "N" (sreg' ! 2)
    z <- newFlag "Z" (sreg' ! 1)
    c <- newFlag "C" (sreg' ! 0)
    aliasFile gpr' 0x00      -- GPR file mapped into data space at 0x00..0x1F
    aliasReg  sp'   0x5D
    aliasReg  sreg' 0x5F
    pure AVRALU
        { avrGPR   = gpr'
        , avrSP    = sp'
        , avrPC    = pc'
        , avrX     = x'
        , avrY     = y'
        , avrZ     = z'
        , avrSREG  = sreg'
        , avrFlagC = c
        , avrFlagZ = z
        , avrFlagN = n
        , avrFlagV = v
        , avrFlagS = s
        , avrFlagH = h
        , avrFlagT = t
        , avrFlagI = i
        }

-- ---------------------------------------------------------------------------
-- Constraint alias
-- pcW pins the PC width; CodeAddr and the PC register share the same width.
-- ---------------------------------------------------------------------------

type AVR m pcW = ( MonadHarvardALU m, AluDef m ~ AVRALU pcW
                 , KnownNat pcW
                 , Word m ~ IExpr (Unsigned 8), DataAddr m ~ IExpr (Unsigned 16)
                 , CodeAddr m ~ IExpr (Unsigned pcW), CodeWord m ~ IExpr (Unsigned 16) )

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Map a 3-bit SREG bit index (0=C .. 7=I) to the corresponding CPUFlag.
avrFlagAt :: AVRALU pcW -> Int -> CPUFlag
avrFlagAt alu 0 = avrFlagC alu
avrFlagAt alu 1 = avrFlagZ alu
avrFlagAt alu 2 = avrFlagN alu
avrFlagAt alu 3 = avrFlagV alu
avrFlagAt alu 4 = avrFlagS alu
avrFlagAt alu 5 = avrFlagH alu
avrFlagAt alu 6 = avrFlagT alu
avrFlagAt alu _ = avrFlagI alu

-- | Set a flag to a compile-time constant via a proper literal wire.
setFlagHi, setFlagLo :: AVR m pcW => CPUFlag -> m ()
setFlagHi f = do { v <- litC 1; setFlag f v }
setFlagLo f = do { v <- litC 0; setFlag f v }

-- | Stub all 6 arithmetic flags to Lo (synthesis placeholder).
stubArith :: AVR m pcW => m ()
stubArith = do
    alu <- cpu id
    mapM_ setFlagLo
        [ avrFlagC alu, avrFlagZ alu, avrFlagN alu
        , avrFlagV alu, avrFlagS alu, avrFlagH alu ]

-- | Advance PC by one instruction word.
pcAdvance :: AVR m pcW => m ()
pcAdvance = do
    pcR <- cpu avrPC
    p   <- readReg pcR
    writeReg pcR (p + 1)

-- | Skip-on-condition: when @cond@ holds, step over the next instruction word
-- (PC += 2); otherwise advance normally (PC += 1).  Used by the SBRC/SBRS/SBIC/
-- SBIS/CPSE skip group.  NB: this skips a single instruction word — a two-word
-- next instruction (LDS/STS/CALL/JMP) would need PC += 3 and is not yet handled.
skipNextIf :: AVR m pcW => IExpr Bool -> m ()
skipNextIf cond = do
    pcR <- cpu avrPC
    p   <- readReg pcR
    one <- litC 1
    two <- litC 2
    writeReg pcR (ifexp cond (p + two) (p + one))
