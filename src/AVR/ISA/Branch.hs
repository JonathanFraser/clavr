{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module AVR.ISA.Branch where

import Prelude hiding (Word)

import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce, slice, add, mul, shiftL, shiftR, xor, (.&.), (.|.))
import Isacle.ISA
import AVR.ISA.Types

-- ---------------------------------------------------------------------------
-- Stack: PUSH, POP
-- ---------------------------------------------------------------------------

-- PUSH Rr — 1001_001d_dddd_1111
instrPUSH :: AVR m pcW => m ()
instrPUSH = do
    mnemonic "PUSH"
    d <- defineInstruction $ do
        fixed "1001001"; d <- field @(Unsigned 5); fixed "1111"; return d
    spV  <- readField avrSP
    val  <- readRegFileF avrGPR d
    writeMem spV val
    one  <- litC 1
    writeField avrSP (spV - one)
    pcAdvance

-- POP Rd — 1001_000d_dddd_1111
instrPOP :: AVR m pcW => m ()
instrPOP = do
    mnemonic "POP"
    d <- defineInstruction $ do
        fixed "1001000"; d <- field @(Unsigned 5); fixed "1111"; return d
    spV   <- readField avrSP
    one   <- litC 1
    let newSp = spV + one
    writeField avrSP newSp
    writeRegFileF avrGPR d =<< readMem newSp
    pcAdvance

-- ---------------------------------------------------------------------------
-- Stack helpers shared by CALL/RET instructions
-- ---------------------------------------------------------------------------

-- | Push a pcW-wide return address onto the stack (hi byte first, AVR convention).
pushRetAddr :: AVR m pcW => IExpr (Unsigned pcW) -> m ()
pushRetAddr ret = do
    spV   <- readField avrSP
    eight <- litC 8
    let retHi = shiftR (zeroExtend ret :: IExpr (Unsigned 16)) eight
    writeMem spV (truncateC retHi :: IExpr (Unsigned 8))   -- 8 <= 16, statically checked
    one   <- litC 1
    let spV1 = spV - one
    writeMem spV1 (truncateB ret :: IExpr (Unsigned 8))
    let spV2 = spV1 - one
    writeField avrSP spV2

-- | Pop a 16-bit return address from the stack and jump to it.
retFromStack :: AVR m pcW => m ()
retFromStack = do
    spV  <- readField avrSP
    one  <- litC 1
    let spV1 = spV + one
    lo   <- readMem spV1
    let spV2 = spV1 + one
    hi   <- readMem spV2
    writeField avrSP spV2
    eight <- litC 8
    let hiW = shiftL (zeroExtendC hi :: IExpr (Unsigned 16)) eight   -- Word m ~ IExpr 8, 8 <= 16
        ret = (zeroExtendC lo :: IExpr (Unsigned 16)) .|. hiW
    writeField avrPC (zeroExtend ret)

-- ---------------------------------------------------------------------------
-- Relative branches
-- These compute the full target PC = (PC+1) + k, so no separate pcAdvance.
-- ---------------------------------------------------------------------------

-- RJMP k — 1100_kkkk_kkkk_kkkk  (12-bit signed offset)
instrRJMP :: forall m pcW. AVR m pcW => m ()
instrRJMP = do
    mnemonic "RJMP"
    k12f <- defineInstruction $ do
        fixed "1100"; k <- field @(Unsigned 12); return k
    p   <- readField avrPC
    one <- litC 1
    let p1 = p + one
    k   <- signExtendBits (immediateF k12f :: IExpr (Unsigned 12))
    writeField avrPC (p1 + k)

-- RCALL k — 1101_kkkk_kkkk_kkkk  (12-bit signed offset, sign-extended)
instrRCALL :: forall m pcW. AVR m pcW => m ()
instrRCALL = do
    mnemonic "RCALL"
    k12f <- defineInstruction $ do
        fixed "1101"; k <- field @(Unsigned 12); return k
    p   <- readField avrPC
    one <- litC 1
    let ret = p + one
    pushRetAddr ret
    k   <- signExtendBits (immediateF k12f :: IExpr (Unsigned 12))
    writeField avrPC (ret + k)

-- RET — 1001_0101_0000_1000
instrRET :: AVR m pcW => m ()
instrRET = do
    mnemonic "RET"
    defineInstruction $ fixed "1001010100001000"
    retFromStack

-- RETI — 1001_0101_0001_1000
instrRETI :: AVR m pcW => m ()
instrRETI = do
    mnemonic "RETI"
    defineInstruction $ fixed "1001010100011000"
    retFromStack
    alu <- cpu id
    setFlagHi (avrFlagI alu)

-- ---------------------------------------------------------------------------
-- Conditional relative branches — BRBS / BRBC
-- Encoding: 1111_00kk_kkkk_ksss (BRBS) / 1111_01kk_kkkk_ksss (BRBC)
-- sss = 3-bit SREG bit index; k = 7-bit signed offset; target = (PC+1) + k.
-- ---------------------------------------------------------------------------

instrBRBS :: forall m pcW. AVR m pcW => m ()
instrBRBS = do
    mnemonic "BRBS"
    (k7f, sssf) <- defineInstruction $ do
        fixed "111100"; k <- field @(Unsigned 7); sss <- field @(Unsigned 3); return (k, sss)
    p      <- readField avrPC
    pcOne  <- litC 1
    let p1 = p + pcOne
    k      <- signExtendBits (immediateF k7f :: IExpr (Unsigned 7))
    let target = p1 + k
    sreg   <- readField avrSREG
    let sss8 = zeroExtendC (immediateF sssf :: IExpr (Unsigned 3)) :: IExpr (Unsigned 8)   -- 3 <= 8
    let shifted = shiftR sreg sss8
    one8    <- litC 1
    let masked = shifted .&. one8
    cond    <- isZero =<< isZero masked   -- 1 when flag is set
    writeField avrPC (ifexp cond target p1)   -- taken: target; not-taken: PC+1

instrBRBC :: forall m pcW. AVR m pcW => m ()
instrBRBC = do
    mnemonic "BRBC"
    (k7f, sssf) <- defineInstruction $ do
        fixed "111101"; k <- field @(Unsigned 7); sss <- field @(Unsigned 3); return (k, sss)
    p      <- readField avrPC
    pcOne  <- litC 1
    let p1 = p + pcOne
    k      <- signExtendBits (immediateF k7f :: IExpr (Unsigned 7))
    let target = p1 + k
    sreg   <- readField avrSREG
    let sss8 = zeroExtendC (immediateF sssf :: IExpr (Unsigned 3)) :: IExpr (Unsigned 8)   -- 3 <= 8
    let shifted = shiftR sreg sss8
    one8    <- litC 1
    let masked = shifted .&. one8
    cond    <- isZero masked              -- 1 when flag is clear
    writeField avrPC (ifexp cond target p1)   -- taken: target; not-taken: PC+1

-- Named aliases — kept for documentation; same encodings as BRBS/BRBC with fixed sss bits.
instrBREQ, instrBRNE :: AVR m pcW => m ()
instrBRCS, instrBRCC :: AVR m pcW => m ()
instrBRMI, instrBRPL :: AVR m pcW => m ()
instrBRVS, instrBRVC :: AVR m pcW => m ()
instrBRLT, instrBRGE :: AVR m pcW => m ()
instrBRHS, instrBRHC :: AVR m pcW => m ()
instrBRTS, instrBRTC :: AVR m pcW => m ()
instrBRIE, instrBRID :: AVR m pcW => m ()
instrBRLO, instrBRSH :: AVR m pcW => m ()
instrBREQ = instrBRBS; instrBRNE = instrBRBC
instrBRCS = instrBRBS; instrBRCC = instrBRBC
instrBRMI = instrBRBS; instrBRPL = instrBRBC
instrBRVS = instrBRBS; instrBRVC = instrBRBC
instrBRLT = instrBRBS; instrBRGE = instrBRBC
instrBRHS = instrBRBS; instrBRHC = instrBRBC
instrBRTS = instrBRBS; instrBRTC = instrBRBC
instrBRIE = instrBRBS; instrBRID = instrBRBC
instrBRLO = instrBRBS; instrBRSH = instrBRBC

-- ---------------------------------------------------------------------------
-- Absolute / indirect control flow + misc
-- ---------------------------------------------------------------------------

-- IJMP — 1001_0100_0000_1001
instrIJMP :: AVR m pcW => m ()
instrIJMP = do
    mnemonic "IJMP"
    defineInstruction $ fixed "1001010000001001"
    writeField avrPC . zeroExtend =<< readField avrZ

-- ICALL — 1001_0101_0000_1001
instrICALL :: AVR m pcW => m ()
instrICALL = do
    mnemonic "ICALL"
    defineInstruction $ fixed "1001010100001001"
    p    <- readField avrPC
    one  <- litC 1
    let ret = p + one
    pushRetAddr ret
    writeField avrPC . zeroExtend =<< readField avrZ

-- NOP — 0000_0000_0000_0000
instrNOP :: AVR m pcW => m ()
instrNOP = do
    mnemonic "NOP"
    defineInstruction $ fixed "0000000000000000"
    pcAdvance

-- ---------------------------------------------------------------------------
-- 32-bit (two-word) control flow.  The k bits in word 1 are unused (16-bit PC
-- uses the second word), so they are don't-cares in the encoding.
-- ---------------------------------------------------------------------------

-- JMP k — 1001_010k_kkkk_110k + 16-bit target word
instrJMP :: AVR m pcW => m ()
instrJMP = do
    mnemonic "JMP"
    defineInstruction $ fixed "1001010.....110."
    p   <- readField avrPC
    tgt <- readCode p
    writeField avrPC (zeroExtend tgt)

-- CALL k — 1001_010k_kkkk_111k + 16-bit target word
instrCALL :: AVR m pcW => m ()
instrCALL = do
    mnemonic "CALL"
    defineInstruction $ fixed "1001010.....111."
    p    <- readField avrPC
    tgt  <- readCode p
    one  <- litC 1
    let p1 = p + one
        ret = p1 + one        -- return address = after both words (p+2)
    pushRetAddr ret
    writeField avrPC (zeroExtend tgt)

-- MOVW Rd+1:Rd, Rr+1:Rr — 0000_0001_dddd_rrrr.  d,r select register /pairs/
-- (actual base = 2·field); copy both bytes, no flags.
instrMOVW :: AVR m pcW => m ()
instrMOVW = do
    mnemonic "MOVW"
    (d, r) <- defineInstruction $ do
        fixed "00000001"; d <- field @(Unsigned 4); r <- field @(Unsigned 4); return (d, r)
    rlo <- readRegFileFScaled avrGPR r 2 0
    rhi <- readRegFileFScaled avrGPR r 2 1
    writeRegFileFScaled avrGPR d 2 0 rlo
    writeRegFileFScaled avrGPR d 2 1 rhi
    pcAdvance
