{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module AVR.ISA.Arith where

import Prelude hiding (Word)

import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce, slice, add, mul, shiftL, shiftR, xor, (.&.), (.|.))
import Isacle.ISA
import AVR.ISA.Types

-- An 8-bit value-typed operation on register contents.
type Op8 = IExpr (Unsigned 8) -> IExpr (Unsigned 8) -> IExpr (Unsigned 8)

type Byte = IExpr (Unsigned 8)
type Flag = IExpr Bool

-- ---------------------------------------------------------------------------
-- Flag computation (SREG: C Z N V S H).  Carry comes from the extra top bit of
-- the width-growing add/sub; half-carry from the low-nibble add/sub; overflow
-- from sign algebra — all computed, none stubbed.
-- ---------------------------------------------------------------------------

bit7 :: Byte -> Flag
bit7 x = slice 7 7 x

-- A 16-bit word value built from / split into byte halves.
type Word16 = IExpr (Unsigned 16)

-- | Pack a high and low byte into a 16-bit word (@256·hi + lo@) — the @·256@ is a
-- left-shift-by-8 the synthesiser folds into pure wiring (see PLAN_16BIT_PACKING).
packBytes :: Byte -> Byte -> Word16
packBytes hi lo = 256 * zeroExtend hi + zeroExtend lo

-- | The low byte of a 16-bit word.
loByte :: Word16 -> Byte
loByte = truncateB

-- | The high byte of a 16-bit word.
hiByte :: Word16 -> Byte
hiByte w = truncateB (shiftR w 8)

readFlag' :: AVR m pcW => (AVRALU pcW -> CPUFlag) -> m Flag
readFlag' sel = cpu sel >>= getFlag

-- | Set C Z N V S H at once.
setCZNVSH :: AVR m pcW => Flag -> Flag -> Flag -> Flag -> Flag -> Flag -> m ()
setCZNVSH c z n v s h = do
    alu <- cpu id
    setFlag (avrFlagC alu) c; setFlag (avrFlagZ alu) z; setFlag (avrFlagN alu) n
    setFlag (avrFlagV alu) v; setFlag (avrFlagS alu) s; setFlag (avrFlagH alu) h

-- | @a + b + cin@, setting all six arithmetic flags; returns the 8-bit result.
addF :: AVR m pcW => Byte -> Byte -> Flag -> m Byte
addF a b cin = do
    let s9   = (zeroExtend a :: IExpr (Unsigned 9)) + zeroExtend b + zeroExtend cin
        res  = truncateB s9 :: Byte
        c    = slice 8 8 s9
        hsum = (a .&. 0x0F) + (b .&. 0x0F) + (zeroExtend cin :: Byte)
        h    = slice 4 4 hsum
        n    = bit7 res
        v    = inv (xor (bit7 a) (bit7 b)) .&. xor (bit7 res) (bit7 a)
    z <- isZero res
    setCZNVSH c z n (v :: Flag) (xor n v) h
    pure res

-- | @a - b - cin@, setting all six flags (C = borrow); returns the result.
subF :: AVR m pcW => Byte -> Byte -> Flag -> m Byte
subF a b cin = do
    let d9   = (zeroExtend a :: IExpr (Unsigned 9)) - zeroExtend b - zeroExtend cin
        res  = truncateB d9 :: Byte
        c    = slice 8 8 d9                                   -- borrow
        hsub = (a .&. 0x0F) - (b .&. 0x0F) - (zeroExtend cin :: Byte)
        h    = slice 4 4 hsub
        n    = bit7 res
        v    = xor (bit7 a) (bit7 b) .&. xor (bit7 a) (bit7 res)
    z <- isZero res
    setCZNVSH c z n (v :: Flag) (xor n v) h
    pure res

-- | A logical result sets Z and N, clears V (S = N); C and H are unchanged.
logicF :: AVR m pcW => Byte -> m Byte
logicF res = do
    alu <- cpu id
    z <- isZero res
    let n = bit7 res
    setFlag (avrFlagZ alu) z; setFlag (avrFlagN alu) n
    setFlag (avrFlagV alu) (0 :: Flag); setFlag (avrFlagS alu) (xor n (0 :: Flag))
    pure res

-- | A two-register arithmetic op that computes its own flags.
twoRegArith :: AVR m pcW => String -> (Byte -> Byte -> m Byte) -> m ()
twoRegArith pre f = do
    (d, r) <- defineInstruction $ twoReg pre
    a <- readRegFileF avrGPR d
    b <- readRegFileF avrGPR r
    res <- f a b
    writeRegFileF avrGPR d res
    pcAdvance

-- | A single-register arithmetic op that computes its own flags.
oneRegArith :: AVR m pcW => String -> (Byte -> m Byte) -> m ()
oneRegArith suf f = do
    d <- defineInstruction $ do
        fixed "1001010"; d <- field @(Unsigned 5); fixed suf; return d
    a <- readRegFileF avrGPR d
    res <- f a
    writeRegFileF avrGPR d res
    pcAdvance

-- INC: Z N V(=res==0x80) S; C and H unchanged.
incF :: AVR m pcW => Byte -> m Byte
incF a = do
    let res = a + 1; n = bit7 res; v = isZeroE (xor res 0x80)
    z <- isZero res; alu <- cpu id
    setFlag (avrFlagZ alu) z; setFlag (avrFlagN alu) n
    setFlag (avrFlagV alu) v; setFlag (avrFlagS alu) (xor n v)
    pure res

-- DEC: Z N V(=res==0x7F) S; C and H unchanged.
decF :: AVR m pcW => Byte -> m Byte
decF a = do
    let res = a - 1; n = bit7 res; v = isZeroE (xor res 0x7F)
    z <- isZero res; alu <- cpu id
    setFlag (avrFlagZ alu) z; setFlag (avrFlagN alu) n
    setFlag (avrFlagV alu) v; setFlag (avrFlagS alu) (xor n v)
    pure res

-- COM: C=1, Z N V=0 S(=N); result is the ones' complement.
comF :: AVR m pcW => Byte -> m Byte
comF a = do
    let res = inv a; n = bit7 res
    z <- isZero res; alu <- cpu id
    setFlag (avrFlagC alu) (1 :: Flag); setFlag (avrFlagZ alu) z; setFlag (avrFlagN alu) n
    setFlag (avrFlagV alu) (0 :: Flag); setFlag (avrFlagS alu) (xor n (0 :: Flag))
    pure res

-- NEG: two's complement; C(=res/=0) Z N V(=res==0x80) S H.
negF :: AVR m pcW => Byte -> m Byte
negF a = do
    let res = 0 - a; n = bit7 res
        c = inv (isZeroE res); v = isZeroE (xor res 0x80)
        h = slice 3 3 res .|. inv (slice 3 3 a)
    z <- isZero res
    setCZNVSH c z n v (xor n v) h
    pure res

-- Right shifts: LSR/ASR/ROR.  C is the bit shifted out (a[0]); V = N xor C.
shiftF :: AVR m pcW => Flag -> Byte -> Byte -> m Byte
shiftF n res a = do
    let c = slice 0 0 a
    z <- isZero res; alu <- cpu id
    setFlag (avrFlagC alu) c; setFlag (avrFlagZ alu) z; setFlag (avrFlagN alu) n
    setFlag (avrFlagV alu) (xor n c); setFlag (avrFlagS alu) (xor n (xor n c))
    pure res

lsrF, asrF, rorF :: AVR m pcW => Byte -> m Byte
lsrF a = shiftF (0 :: Flag) (shiftR a 1) a
asrF a = let res = arithShiftR a 1 in shiftF (bit7 res) res a
rorF a = do
    cin <- readFlag' avrFlagC
    let res = shiftL (zeroExtend cin :: Byte) 7 .|. shiftR a 1
    shiftF (bit7 res) res a

-- ---------------------------------------------------------------------------
-- Two-register arithmetic/logical instructions  ("00XX_XXrd_dddd_rrrr")
-- The "<6 fixed bits>rd_dddd_rrrr" shape is captured by 'twoReg'.
-- ---------------------------------------------------------------------------

-- | A two-register op: read Rd and Rr, combine with @f@, write Rd.
twoRegOp :: AVR m pcW => String -> Op8 -> m ()
twoRegOp pre f = do
    (d, r) <- defineInstruction $ twoReg pre
    a <- readRegFileF avrGPR d
    b <- readRegFileF avrGPR r
    writeRegFileF avrGPR d (f a b)
    stubArith
    pcAdvance

-- | A two-register compare (no write-back): the difference drives the flags.
twoRegCmp :: AVR m pcW => String -> (Byte -> Byte -> m Byte) -> m ()
twoRegCmp pre f = do
    (d, r) <- defineInstruction $ twoReg pre
    a <- readRegFileF avrGPR d
    b <- readRegFileF avrGPR r
    _ <- f a b                 -- sets flags; result discarded
    pcAdvance

instrADD, instrADC, instrSUB, instrSBC, instrAND, instrOR, instrEOR :: AVR m pcW => m ()
instrADD = mnemonic "ADD" >> twoRegArith "000011" (\a b -> addF a b (0 :: Flag))
instrADC = mnemonic "ADC" >> twoRegArith "000111" (\a b -> readFlag' avrFlagC >>= addF a b)
instrSUB = mnemonic "SUB" >> twoRegArith "000110" (\a b -> subF a b (0 :: Flag))
instrSBC = mnemonic "SBC" >> twoRegArith "000010" (\a b -> readFlag' avrFlagC >>= subF a b)
instrAND = mnemonic "AND" >> twoRegArith "001000" (\a b -> logicF (a .&. b))
instrOR  = mnemonic "OR"  >> twoRegArith "001010" (\a b -> logicF (a .|. b))
instrEOR = mnemonic "EOR" >> twoRegArith "001001" (\a b -> logicF (xor a b))

instrCP, instrCPC :: AVR m pcW => m ()
instrCP   = mnemonic "CP"   >> twoRegCmp "000101" (\a b -> subF a b (0 :: Flag))
instrCPC  = mnemonic "CPC"  >> twoRegCmp "000001" (\a b -> readFlag' avrFlagC >>= subF a b)

-- CPSE Rd, Rr — 0001_00rd_dddd_rrrr : skip next if Rd == Rr (does not touch flags)
instrCPSE :: AVR m pcW => m ()
instrCPSE = do
    mnemonic "CPSE"
    (d, r) <- defineInstruction $ twoReg "000100"
    a <- readRegFileF avrGPR d
    b <- readRegFileF avrGPR r
    skipNextIf (isZeroE (xor a b))   -- equal ⇒ skip

-- MOV Rd, Rr — 0010_11rd_dddd_rrrr
instrMOV :: AVR m pcW => m ()
instrMOV = do
    mnemonic "MOV"
    (d, r) <- defineInstruction $ twoReg "001011"
    writeRegFileF avrGPR d =<< readRegFileF avrGPR r
    pcAdvance

-- MUL Rd, Rr — 1001_11rd_dddd_rrrr  (unsigned 8×8 → 16-bit product in R1:R0)
-- The product is genuinely 16 bits: 'mul' grows the result type to Unsigned 16,
-- and its low/high bytes go to R0/R1 (fixed result registers, by constant index).
instrMUL :: AVR m pcW => m ()
instrMUL = do
    mnemonic "MUL"
    (d, r) <- defineInstruction $ twoReg "100111"
    a <- readRegFileF avrGPR d
    b <- readRegFileF avrGPR r
    let p = mul a b :: IExpr (Unsigned 16)
    writeRegFileAt avrGPR 0 (truncateB p)     -- R0 = product[7:0]
    writeRegFileAt avrGPR 1 (slice 15 8 p)    -- R1 = product[15:8]
    mulFlags p
    pcAdvance

-- MUL/MULS set C = product bit 15 and Z = product == 0.
mulFlags :: AVR m pcW => IExpr (Unsigned 16) -> m ()
mulFlags p = do
    alu <- cpu id
    z <- isZero p
    setFlag (avrFlagC alu) (slice 15 15 p)
    setFlag (avrFlagZ alu) z

-- ---------------------------------------------------------------------------
-- Upper-register + 8-bit immediate instructions  ("XXXX_KKKK_dddd_KKKK")
-- The "<4 fixed bits>KKKK_dddd_KKKK" shape (Rd+16, split K) is 'immReg'.
-- ---------------------------------------------------------------------------

-- | An upper-register immediate op: read Rd (R16–R31), combine with K via the
-- flag-computing @f@, write Rd.  Same helpers as the register form — the only
-- difference is that the second operand is the immediate field.
immRegArith :: AVR m pcW => String -> (Byte -> Byte -> m Byte) -> m ()
immRegArith pre f = do
    (d, k) <- defineInstruction $ immReg pre
    a <- readRegFileFOffset avrGPR d 16
    res <- f a (immediateF k)
    writeRegFileFOffset avrGPR d 16 res
    pcAdvance

-- | An upper-register immediate compare (no write-back).
immRegCmp :: AVR m pcW => String -> (Byte -> Byte -> m Byte) -> m ()
immRegCmp pre f = do
    (d, k) <- defineInstruction $ immReg pre
    a <- readRegFileFOffset avrGPR d 16
    _ <- f a (immediateF k)
    pcAdvance

instrSUBI, instrSBCI, instrANDI, instrORI :: AVR m pcW => m ()
instrSUBI = mnemonic "SUBI" >> immRegArith "0101" (\a k -> subF a k (0 :: Flag))
instrSBCI = mnemonic "SBCI" >> immRegArith "0100" (\a k -> readFlag' avrFlagC >>= subF a k)
instrANDI = mnemonic "ANDI" >> immRegArith "0111" (\a k -> logicF (a .&. k))
instrORI  = mnemonic "ORI"  >> immRegArith "0110" (\a k -> logicF (a .|. k))

-- LDI Rd, K — 1110_KKKK_dddd_KKKK  (no flags)
instrLDI :: AVR m pcW => m ()
instrLDI = do
    mnemonic "LDI"
    (d, k) <- defineInstruction $ immReg "1110"
    writeRegFileFOffset avrGPR d 16 (immediateF k)
    pcAdvance

-- CPI Rd, K — 0011_KKKK_dddd_KKKK  (compare, no write-back)
instrCPI :: AVR m pcW => m ()
instrCPI = mnemonic "CPI" >> immRegCmp "0011" (\a k -> subF a k (0 :: Flag))

-- ---------------------------------------------------------------------------
-- Single-register instructions  ("1001_010d_dddd_XXXX", contiguous 5-bit d)
-- ---------------------------------------------------------------------------

-- | A single-register op given the 4-bit suffix and a pure transform of Rd.
oneRegOp :: AVR m pcW => String -> (IExpr (Unsigned 8) -> IExpr (Unsigned 8)) -> m ()
oneRegOp suf f = do
    d <- defineInstruction $ do
        fixed "1001010"; d <- field @(Unsigned 5); fixed suf; return d
    a <- readRegFileF avrGPR d
    writeRegFileF avrGPR d (f a)
    stubArith
    pcAdvance

instrINC, instrCOM, instrNEG, instrASR, instrLSR, instrROR :: AVR m pcW => m ()
instrINC = mnemonic "INC" >> oneRegArith "0011" incF
instrCOM = mnemonic "COM" >> oneRegArith "0000" comF
instrNEG = mnemonic "NEG" >> oneRegArith "0001" negF
instrASR = mnemonic "ASR" >> oneRegArith "0101" asrF
instrLSR = mnemonic "LSR" >> oneRegArith "0110" lsrF
instrROR = mnemonic "ROR" >> oneRegArith "0111" rorF

-- SWAP Rd — 1001_010d_dddd_0010
instrSWAP :: AVR m pcW => m ()
instrSWAP = do
    mnemonic "SWAP"
    d <- defineInstruction $ do
        fixed "1001010"; d <- field @(Unsigned 5); fixed "0010"; return d
    a <- readRegFileF avrGPR d
    let hi = shiftR a 4
        lo = shiftL a 4
    writeRegFileF avrGPR d (hi .|. lo)
    pcAdvance

-- DEC Rd — 1001_010d_dddd_1010  (Z N V S; C and H unchanged)
instrDEC :: AVR m pcW => m ()
instrDEC = mnemonic "DEC" >> oneRegArith "1010" decF

-- ---------------------------------------------------------------------------
-- MULS — signed multiply upper regs — 0000_0010_dddd_rrrr  (d+16, r+16)
-- Same shape as MUL, but the operands are reinterpreted as signed before the
-- (now signed) growing multiply — the only difference between MUL and MULS is
-- the operand /type/, not the opcode.
-- ---------------------------------------------------------------------------

instrMULS :: AVR m pcW => m ()
instrMULS = do
    mnemonic "MULS"
    (d, r) <- defineInstruction $ do
        fixed "00000010"; d <- field @(Unsigned 4); r <- field @(Unsigned 4); return (d, r)
    a <- readRegFileFOffset avrGPR d 16
    b <- readRegFileFOffset avrGPR r 16
    let p = asUnsigned (mul (asSigned a) (asSigned b)) :: IExpr (Unsigned 16)
    writeRegFileAt avrGPR 0 (truncateB p)
    writeRegFileAt avrGPR 1 (slice 15 8 p)
    mulFlags p
    pcAdvance

-- ---------------------------------------------------------------------------
-- ADIW / SBIW — wide immediate add/sub on register pairs
-- 1001_011X_KKdd_KKKK  (X=0 ADIW, X=1 SBIW; d=2-bit pair selector → 24+2*d).
-- ---------------------------------------------------------------------------

-- | The wide-immediate shape: 6-bit K split (KK high, KKKK low), 2-bit pair dd.
adiwEnc :: String -> Encoding (Field (Unsigned 2), Field (Unsigned 6))
adiwEnc pre = do
    fixed pre
    k <- placeholder @(Unsigned 6)
    bindBits k 2            -- KK (bits 7-6)
    d <- field @(Unsigned 2)  -- dd (bits 5-4)
    bindBits k 4            -- KKKK (bits 3-0)
    return (d, k)

-- | Set C Z N V S (the wide-op flag set); H is /unchanged/ (unlike 'setCZNVSH').
setCZNVS :: AVR m pcW => Flag -> Flag -> Flag -> Flag -> m ()
setCZNVS c z n v = do
    alu <- cpu id
    setFlag (avrFlagC alu) c; setFlag (avrFlagZ alu) z; setFlag (avrFlagN alu) n
    setFlag (avrFlagV alu) v; setFlag (avrFlagS alu) (xor n v)

-- | Read the @24 + 2·d@ register pair as a 16-bit word (high byte = @+1@ entry).
readPair :: AVR m pcW => Field (Unsigned 2) -> m Word16
readPair d = do
    lo <- readRegFileFScaled avrGPR d 2 24
    hi <- readRegFileFScaled avrGPR d 2 25
    pure (packBytes hi lo)

-- | Write a 16-bit word back to the @24 + 2·d@ register pair.
writePair :: AVR m pcW => Field (Unsigned 2) -> Word16 -> m ()
writePair d w = do
    writeRegFileFScaled avrGPR d 2 24 (loByte w)
    writeRegFileFScaled avrGPR d 2 25 (hiByte w)

bit15 :: Word16 -> Flag
bit15 = slice 15 15

-- ADIW Rd+1:Rd, K — 16-bit add of the zero-extended 6-bit immediate to the pair.
-- C = carry out of bit 15; V = !Rdh7·R15; N = R15; Z = (result == 0); S = N⊕V.
instrADIW :: AVR m pcW => m ()
instrADIW = do
    mnemonic "ADIW"
    (d, k) <- defineInstruction $ adiwEnc "10010110"
    pair <- readPair d
    let s17 = (zeroExtend pair :: IExpr (Unsigned 17))
              + zeroExtend (immediateF k :: IExpr (Unsigned 6))
        res = truncateB s17 :: Word16
        c   = slice 16 16 s17
        n   = bit15 res
        v   = inv (bit15 pair) .&. bit15 res
    z <- isZero res
    writePair d res
    setCZNVS c z n v
    pcAdvance

-- SBIW Rd+1:Rd, K — 16-bit subtract.  C = borrow (bit 16 of the widened diff);
-- V = Rdh7·!R15; N = R15; Z = (result == 0); S = N⊕V.
instrSBIW :: AVR m pcW => m ()
instrSBIW = do
    mnemonic "SBIW"
    (d, k) <- defineInstruction $ adiwEnc "10010111"
    pair <- readPair d
    let wdiff = (zeroExtend pair :: IExpr (Unsigned 17))
                - zeroExtend (immediateF k :: IExpr (Unsigned 6))
        res = truncateB wdiff :: Word16
        c   = slice 16 16 wdiff
        n   = bit15 res
        v   = bit15 pair .&. inv (bit15 res)
    z <- isZero res
    writePair d res
    setCZNVS c z n v
    pcAdvance
