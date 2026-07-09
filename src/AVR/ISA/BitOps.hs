{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module AVR.ISA.BitOps where

import Prelude hiding (Word)

import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce, slice, add, mul, shiftL, shiftR, xor, (.&.), (.|.))
import Isacle.ISA
import AVR.ISA.Types

-- ---------------------------------------------------------------------------
-- Bit manipulation: BSET, BCLR, BST, BLD
-- ---------------------------------------------------------------------------

-- BSET s — 1001_0100_0sss_1000
instrBSET :: AVR m pcW => m ()
instrBSET = do
    mnemonic "BSET"
    -- SREG[sss] := 1, where sss is a runtime field:  SREG <- SREG | (1 << sss)
    s <- defineInstruction $ do
        fixed "100101000"; s <- field @(Unsigned 3); fixed "1000"; return s
    sreg <- readField avrSREG
    one  <- litC 1
    let mask = shiftL one (zeroExtendC (immediateF s :: IExpr (Unsigned 3)) :: IExpr (Unsigned 8))
    writeField avrSREG (sreg .|. mask)
    pcAdvance

-- BCLR s — 1001_0100_1sss_1000
instrBCLR :: AVR m pcW => m ()
instrBCLR = do
    mnemonic "BCLR"
    -- SREG[sss] := 0:  SREG <- SREG & ~(1 << sss)
    s <- defineInstruction $ do
        fixed "100101001"; s <- field @(Unsigned 3); fixed "1000"; return s
    sreg    <- readField avrSREG
    one     <- litC 1
    let mask    = shiftL one (zeroExtendC (immediateF s :: IExpr (Unsigned 3)) :: IExpr (Unsigned 8))
        notMask = inv mask
    writeField avrSREG (sreg .&. notMask)
    pcAdvance

-- | Build the one-hot mask @1 << b@ for a 3-bit bit-number field.
bitMask :: Field (Unsigned 3) -> IExpr (Unsigned 8)
bitMask b = shiftL 1 (zeroExtend (immediateF b) :: IExpr (Unsigned 8))

-- | 1 when bit @b@ of @v@ is set, 0 when clear.
bitIsSet :: IExpr (Unsigned 8) -> Field (Unsigned 3) -> IExpr Bool
bitIsSet v b = inv (isZeroE (v .&. bitMask b))

-- BST Rd, b — 1111_101d_dddd_0bbb :  T := bit b of Rd
instrBST :: AVR m pcW => m ()
instrBST = do
    mnemonic "BST"
    (d, b) <- defineInstruction $ do
        fixed "1111101"; d <- field @(Unsigned 5); fixed "0"; b <- field @(Unsigned 3); return (d, b)
    v   <- readRegFileF avrGPR d
    alu <- cpu id
    let bn = zeroExtend (immediateF b) :: IExpr (Unsigned 8)
    setFlag (avrFlagT alu) (slice 0 0 (shiftR v bn))   -- bit b → bit 0 → T
    pcAdvance

-- BLD Rd, b — 1111_100d_dddd_0bbb :  bit b of Rd := T
instrBLD :: AVR m pcW => m ()
instrBLD = do
    mnemonic "BLD"
    (d, b) <- defineInstruction $ do
        fixed "1111100"; d <- field @(Unsigned 5); fixed "0"; b <- field @(Unsigned 3); return (d, b)
    v <- readRegFileF avrGPR d
    t <- readFlag avrFlagT
    let bn   = zeroExtend (immediateF b) :: IExpr (Unsigned 8)
        m    = bitMask b
    writeRegFileF avrGPR d ((v .&. inv m) .|. shiftL (zeroExtend t :: IExpr (Unsigned 8)) bn)
    pcAdvance

-- ---------------------------------------------------------------------------
-- Skip-if-bit: SBRC, SBRS, SBIC, SBIS, CBI, SBI
-- The skip-group instructions step over the next instruction word when their
-- bit test passes (see 'skipNextIf').
-- ---------------------------------------------------------------------------

-- SBRC Rr, b — 1111_110r_rrrr_0bbb : skip next if bit b of Rr is clear
instrSBRC :: AVR m pcW => m ()
instrSBRC = do
    mnemonic "SBRC"
    (r, b) <- defineInstruction $ do
        fixed "1111110"; r <- field @(Unsigned 5); fixed "0"; b <- field @(Unsigned 3); return (r, b)
    v <- readRegFileF avrGPR r
    skipNextIf (inv (bitIsSet v b))

-- SBRS Rr, b — 1111_111r_rrrr_0bbb : skip next if bit b of Rr is set
instrSBRS :: AVR m pcW => m ()
instrSBRS = do
    mnemonic "SBRS"
    (r, b) <- defineInstruction $ do
        fixed "1111111"; r <- field @(Unsigned 5); fixed "0"; b <- field @(Unsigned 3); return (r, b)
    v <- readRegFileF avrGPR r
    skipNextIf (bitIsSet v b)

-- CBI A, b — 1001_1000_AAAA_Abbb :  clear bit b of I/O register A
instrCBI :: AVR m pcW => m ()
instrCBI = do
    mnemonic "CBI"
    (a, b) <- defineInstruction $ do
        fixed "10011000"; a <- field @(Unsigned 5); b <- field @(Unsigned 3); return (a, b)
    ioBase <- litC 0x20
    let addr = (zeroExtendC (immediateF a :: IExpr (Unsigned 5)) :: IExpr (Unsigned 16)) + ioBase
    v <- readMem addr
    writeMem addr (v .&. inv (bitMask b))
    pcAdvance

-- SBI A, b — 1001_1010_AAAA_Abbb :  set bit b of I/O register A
instrSBI :: AVR m pcW => m ()
instrSBI = do
    mnemonic "SBI"
    (a, b) <- defineInstruction $ do
        fixed "10011010"; a <- field @(Unsigned 5); b <- field @(Unsigned 3); return (a, b)
    ioBase <- litC 0x20
    let addr = (zeroExtendC (immediateF a :: IExpr (Unsigned 5)) :: IExpr (Unsigned 16)) + ioBase
    v <- readMem addr
    writeMem addr (v .|. bitMask b)
    pcAdvance

-- SBIC A, b — 1001_1001_AAAA_Abbb : skip next if bit b of I/O register A is clear
instrSBIC :: AVR m pcW => m ()
instrSBIC = do
    mnemonic "SBIC"
    (a, b) <- defineInstruction $ do
        fixed "10011001"; a <- field @(Unsigned 5); b <- field @(Unsigned 3); return (a, b)
    ioBase <- litC 0x20
    let addr = (zeroExtendC (immediateF a :: IExpr (Unsigned 5)) :: IExpr (Unsigned 16)) + ioBase
    v   <- readMem addr
    skipNextIf (inv (bitIsSet v b))

-- SBIS A, b — 1001_1011_AAAA_Abbb : skip next if bit b of I/O register A is set
instrSBIS :: AVR m pcW => m ()
instrSBIS = do
    mnemonic "SBIS"
    (a, b) <- defineInstruction $ do
        fixed "10011011"; a <- field @(Unsigned 5); b <- field @(Unsigned 3); return (a, b)
    ioBase <- litC 0x20
    let addr = (zeroExtendC (immediateF a :: IExpr (Unsigned 5)) :: IExpr (Unsigned 16)) + ioBase
    v   <- readMem addr
    skipNextIf (bitIsSet v b)

-- ---------------------------------------------------------------------------
-- IN / OUT — I/O space access (I/O address + 0x20 = data address)
-- The 6-bit I/O address is split in the encoding (AA high, AAAA low), placed
-- with two 'slice's of one placeholder.
-- ---------------------------------------------------------------------------

-- IN Rd, A — 1011_0AAd_dddd_AAAA
instrIN :: AVR m pcW => m ()
instrIN = do
    mnemonic "IN"
    (d, a) <- defineInstruction $ do
        fixed "10110"
        a <- placeholder @(Unsigned 6)
        bindBits a 2                       -- AA: bits 10-9
        d <- field @(Unsigned 5)        -- d:  bits 8-4
        bindBits a 4                       -- AAAA: bits 3-0
        return (d, a)
    ioBase <- litC 0x20
    let addr = (zeroExtendC (immediateF a :: IExpr (Unsigned 6)) :: IExpr (Unsigned 16)) + ioBase
    v    <- readMem addr
    writeRegFileF avrGPR d v
    pcAdvance

-- OUT A, Rr — 1011_1AAr_rrrr_AAAA
instrOUT :: AVR m pcW => m ()
instrOUT = do
    mnemonic "OUT"
    (r, a) <- defineInstruction $ do
        fixed "10111"
        a <- placeholder @(Unsigned 6)
        bindBits a 2                       -- AA: bits 10-9
        r <- field @(Unsigned 5)        -- r:  bits 8-4
        bindBits a 4                       -- AAAA: bits 3-0
        return (r, a)
    ioBase <- litC 0x20
    let addr = (zeroExtendC (immediateF a :: IExpr (Unsigned 6)) :: IExpr (Unsigned 16)) + ioBase
    v    <- readRegFileF avrGPR r
    writeMem addr v
    pcAdvance
