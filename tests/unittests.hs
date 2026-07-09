{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Per-instruction state+flag regression for the AVR core, run over the Sim
-- backend (which exposes the whole architectural state).  For each instruction
-- we compare the Sim result against a /reference model/ of the documented AVR
-- semantics — result register AND every affected SREG flag.  Instructions whose
-- flag logic is still stubbed therefore show up here as real, tracked failures
-- rather than passing silently.
module Main where

import Prelude
import Data.Bits (testBit, (.&.), (.|.), shiftL, shiftR, xor, complement)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import System.Exit (exitFailure, exitSuccess)

import Isacle.ISA (runCPUDef)
import Isacle.ISA.Build (ISABuild)
import Isacle.ISA.Backend.Sim (SimState(..), SimCPU(..), emptySim, runInstr)
import AVR.ISA

-- The Sim monad specialised to the AVR (8-bit data word, 16-bit addresses/PC).
type AvrSim = ISABuild (AVRALU 16) 8 16 16 16

alu :: AVRALU 16
alu = fst (runCPUDef (avrCPUDef @16))

-- Run one instruction body (with its opcode word) from an initial register/flag
-- map; return the resulting register map (entries keyed "GPR:n", "SREG", …).
runOp :: AvrSim () -> Integer -> [(String, Integer)] -> Map String Integer
runOp body word inits =
    scRegs (ssCPU (runInstr alu word body (emptySim { ssCPU = SimCPU (Map.fromList inits) })))

regOf :: Map String Integer -> String -> Integer
regOf m k = Map.findWithDefault 0 k m

-- SREG bit positions (AVR): C0 Z1 N2 V3 S4 H5 T6 I7.
flagBit :: String -> Int
flagBit f = maybe (error f) id (lookup f [("C",0),("Z",1),("N",2),("V",3),("S",4),("H",5),("T",6),("I",7)])

flagOf :: Map String Integer -> String -> Int
flagOf m f = if testBit (regOf m "SREG") (flagBit f) then 1 else 0

-- ---------------------------------------------------------------------------
-- Reference model of the documented AVR semantics (result + affected flags)
-- ---------------------------------------------------------------------------

msb :: Int -> Int
msb x = if testBit x 7 then 1 else 0

-- (result, [(flag, expected value)]) for the flags the instruction affects.
refAdd :: Int -> Int -> Int -> (Int, [(String, Int)])
refAdd a b cin =
    let r = a + b + cin; res = r .&. 0xFF
        c = if r > 0xFF then 1 else 0
        h = if (a .&. 0xF) + (b .&. 0xF) + cin > 0xF then 1 else 0
        n = msb res; z = if res == 0 then 1 else 0
        v = if msb a == msb b && msb res /= msb a then 1 else 0
    in (res, [("C",c),("Z",z),("N",n),("V",v),("S",n `xor` v),("H",h)])

refSub :: Int -> Int -> Int -> (Int, [(String, Int)])
refSub a b cin =
    let r = a - b - cin; res = r .&. 0xFF
        c = if b + cin > a then 1 else 0
        h = if (a .&. 0xF) < (b .&. 0xF) + cin then 1 else 0
        n = msb res; z = if res == 0 then 1 else 0
        v = if msb a /= msb b && msb res /= msb a then 1 else 0
    in (res, [("C",c),("Z",z),("N",n),("V",v),("S",n `xor` v),("H",h)])

refLogic :: (Int -> Int -> Int) -> Int -> Int -> (Int, [(String, Int)])
refLogic op a b =
    let res = op a b .&. 0xFF
        n = msb res; z = if res == 0 then 1 else 0
    in (res, [("Z",z),("N",n),("V",0),("S",n)])

refCom :: Int -> (Int, [(String, Int)])
refCom a = let res = complement a .&. 0xFF; n = msb res
           in (res, [("C",1),("Z",if res==0 then 1 else 0),("N",n),("V",0),("S",n)])

refNeg :: Int -> (Int, [(String, Int)])
refNeg a = let res = (0 - a) .&. 0xFF; n = msb res; v = if res == 0x80 then 1 else 0
           in (res, [("C",if res/=0 then 1 else 0),("Z",if res==0 then 1 else 0)
                    ,("N",n),("V",v),("S",n `xor` v)])

refInc :: Int -> (Int, [(String, Int)])
refInc a = let res = (a+1) .&. 0xFF; n = msb res; v = if res==0x80 then 1 else 0
           in (res, [("Z",if res==0 then 1 else 0),("N",n),("V",v),("S",n `xor` v)])

refDec :: Int -> (Int, [(String, Int)])
refDec a = let res = (a-1) .&. 0xFF; n = msb res; v = if res==0x7F then 1 else 0
           in (res, [("Z",if res==0 then 1 else 0),("N",n),("V",v),("S",n `xor` v)])

refLsr :: Int -> (Int, [(String, Int)])
refLsr a = let res = a `shiftR` 1; c = a .&. 1; n = 0
           in (res, [("C",c),("Z",if res==0 then 1 else 0),("N",n),("V",n `xor` c),("S",n `xor` (n `xor` c))])

refAsr :: Int -> (Int, [(String, Int)])
refAsr a = let res = (a `shiftR` 1) .|. (a .&. 0x80); c = a .&. 1; n = msb res
           in (res, [("C",c),("Z",if res==0 then 1 else 0),("N",n),("V",n `xor` c),("S",n `xor` (n `xor` c))])

refRor :: Int -> Int -> (Int, [(String, Int)])
refRor a cin = let res = ((cin `shiftL` 7) .|. (a `shiftR` 1)) .&. 0xFF; c = a .&. 1; n = msb res
               in (res, [("C",c),("Z",if res==0 then 1 else 0),("N",n),("V",n `xor` c),("S",n `xor` (n `xor` c))])

refSwap :: Int -> (Int, [(String, Int)])
refSwap a = (((a .&. 0xF) `shiftL` 4) .|. (a `shiftR` 4), [])

refMov :: Int -> (Int, [(String, Int)])
refMov b = (b, [])

-- ---------------------------------------------------------------------------
-- Test specs: each is (mnemonic, body, opcode, kind) over sample operands.
-- Rd = R16 ("GPR:16"), Rr = R17 ("GPR:17"); carry-in via SREG bit 0.
-- ---------------------------------------------------------------------------

data Kind
    = Bin  (Int -> Int -> Int -> (Int, [(String,Int)])) Bool  -- a b cin -> …; usesCarryIn
    | Un   (Int -> (Int, [(String,Int)]))                     -- a -> …
    | UnC  (Int -> Int -> (Int, [(String,Int)]))             -- a cin -> …  (ROR)
    | MovK (Int -> (Int, [(String,Int)]))                    -- result from Rr

data Spec = Spec String (AvrSim ()) Integer Kind

samplesBin :: [(Int,Int,Int)]
samplesBin = [(0x2E,0x12,0),(0xF0,0x20,0),(0x50,0x50,0),(0x00,0x00,0),(0x3A,0x3A,1)]

samplesUn :: [Int]
samplesUn = [0x00,0x01,0x80,0xFF,0x10,0x7F]

specs :: [Spec]
specs =
    [ Spec "ADD"  instrADD  0x0F01 (Bin refAdd False)
    , Spec "ADC"  instrADC  0x1F01 (Bin refAdd True)
    , Spec "SUB"  instrSUB  0x1B01 (Bin refSub False)
    , Spec "SBC"  instrSBC  0x0B01 (Bin refSub True)
    , Spec "AND"  instrAND  0x2301 (Bin (\a b _ -> refLogic (.&.) a b) False)
    , Spec "OR"   instrOR   0x2B01 (Bin (\a b _ -> refLogic (.|.) a b) False)
    , Spec "EOR"  instrEOR  0x2701 (Bin (\a b _ -> refLogic xor a b) False)
    , Spec "COM"  instrCOM  0x9500 (Un refCom)
    , Spec "NEG"  instrNEG  0x9501 (Un refNeg)
    , Spec "INC"  instrINC  0x9503 (Un refInc)
    , Spec "DEC"  instrDEC  0x950A (Un refDec)
    , Spec "LSR"  instrLSR  0x9506 (Un refLsr)
    , Spec "ASR"  instrASR  0x9505 (Un refAsr)
    , Spec "ROR"  instrROR  0x9507 (UnC refRor)
    , Spec "SWAP" instrSWAP 0x9502 (Un refSwap)
    , Spec "MOV"  instrMOV  0x2F01 (MovK refMov)
    ]

-- One (label, ok) per checked field, for one operand sample.
checkSample :: Spec -> (Int, Int, Int) -> [(String, Bool, String)]
checkSample (Spec nm body word kind) (a, b, cin) =
    let (expRes, expFlags, inits, opdesc) = case kind of
            Bin ref ucin ->
                let cin' = if ucin then cin else 0
                    (r, fs) = ref a b cin'
                in (r, fs, [("GPR:16",fromIntegral a),("GPR:17",fromIntegral b)]
                        ++ [("SREG", fromIntegral cin') | ucin], show a ++ "," ++ show b ++ (if ucin then " cin="++show cin' else ""))
            Un ref  -> let (r, fs) = ref a in (r, fs, [("GPR:16",fromIntegral a)], show a)
            UnC ref -> let (r, fs) = ref a cin in (r, fs, [("GPR:16",fromIntegral a),("SREG",fromIntegral cin)], show a ++ " cin="++show cin)
            MovK ref-> let (r, fs) = ref b in (r, fs, [("GPR:16",fromIntegral a),("GPR:17",fromIntegral b)], "Rr="++show b)
        m = runOp body word inits
        lbl s = nm ++ "[" ++ opdesc ++ "] " ++ s
        actRes = fromIntegral (regOf m "GPR:16")
        resChk = (lbl ("Rd=" ++ show expRes), actRes == expRes, "got " ++ show actRes)
        flagChk (f, ev) = let av = flagOf m f in (lbl (f ++ "=" ++ show ev), av == ev, "got " ++ show av)
    in resChk : map flagChk expFlags

-- ===========================================================================
-- Scenario harness: covers every remaining AVR instruction by setting up a full
-- SimState (registers, SREG, data memory, code memory, PC, SP), running the
-- instruction body, and asserting the documented AVR post-state.  Where the
-- core's body is still a stub (e.g. BST/BLD/SBRC/CPSE skip, ADIW 16-bit math,
-- MOVW pair copy, immediate-op flags) the assertion is written against the
-- CORRECT AVR semantics, so the stub shows up as a tracked failure.
-- ===========================================================================

-- A scenario: label, list of opcode words (word0 -> runInstr, word1.. -> code
-- mem at PC+0,PC+1,..), initial register assoc, initial data memory, expected
-- checks computed from the resulting SimState.
data Scenario = Scenario
    { scName   :: String
    , scBody   :: AvrSim ()                 -- instruction body under test
    , scWords  :: [Integer]                 -- word0 is the opcode; rest are extension words
    , scRegs0  :: [(String, Integer)]       -- initial register/flag state
    , scMem0   :: [(Int, Integer)]          -- initial data memory
    , scChecks :: SimState -> [(String, Bool, String)]
    }

-- Build the SREG byte from a list of (flagName, 0/1).
sregOf :: [(String, Int)] -> Integer
sregOf = foldr (\(f,v) acc -> if v /= 0 then acc .|. (1 `shiftL` flagBit f) else acc) 0

-- Run a scenario: word0 into runInstr; words 1.. placed in code memory at the
-- locations the two-word bodies read (readCode is at PC, PC+1, ...; PC defaults
-- to whatever scRegs0 sets, else 0).  We place every extension word starting at
-- PC so both "readCode PC" and "readCode (PC+1)" styles are satisfied.
runScenario :: Scenario -> SimState
runScenario sc =
    let st0 = emptySim
                { ssCPU     = SimCPU (Map.fromList (scRegs0 sc))
                , ssDataMem = IntMap.fromList [ (a,v) | (a,v) <- scMem0 sc ]
                , ssCodeMem = IntMap.fromList (zip [pc0 ..] codeWords)
                }
    in case scWords sc of
         []       -> st0
         (w0:_)   -> runInstr alu w0 (scBody sc) st0
  where
    pc0       = fromIntegral (maybe 0 id (lookup "PC" (scRegs0 sc)))
    -- extension words placed at PC, PC+1, ... (covers both readCode conventions)
    codeWords = case scWords sc of
                  (_:rest) -> rest
                  []       -> []

-- Check helpers over a resulting SimState.
type Chk = SimState -> [(String, Bool, String)]

stReg :: SimState -> String -> Integer
stReg st k = Map.findWithDefault 0 k (scRegs (ssCPU st))

stFlag :: SimState -> String -> Int
stFlag st f = if testBit (stReg st "SREG") (flagBit f) then 1 else 0

stMem :: SimState -> Int -> Integer
stMem st a = IntMap.findWithDefault 0 a (ssDataMem st)

chkReg :: String -> String -> Integer -> Chk
chkReg lbl k expv st =
    let got = stReg st k in [(lbl ++ " " ++ k ++ "=" ++ show expv, got == expv, "got " ++ show got)]

chkFlag :: String -> String -> Int -> Chk
chkFlag lbl f expv st =
    let got = stFlag st f in [(lbl ++ " " ++ f ++ "=" ++ show expv, got == expv, "got " ++ show got)]

chkMem :: String -> Int -> Integer -> Chk
chkMem lbl a expv st =
    let got = stMem st a in [(lbl ++ " mem[" ++ show a ++ "]=" ++ show expv, got == expv, "got " ++ show got)]

chkFlags :: String -> [(String, Int)] -> Chk
chkFlags lbl fs st = concat [ chkFlag lbl f v st | (f,v) <- fs ]

-- Sequence several check-builders against the same SimState.
manyChk :: [Chk] -> Chk
manyChk cs st = concatMap ($ st) cs

-- ---------------------------------------------------------------------------
-- The scenario list.  Rd = R16 ("GPR:16"), Rr = R17 ("GPR:17"); the wide ops
-- use the R24:R25 pair (ADIW/SBIW r24).  All flag expectations are the CORRECT
-- AVR values, so stubbed bodies fail visibly.
-- ---------------------------------------------------------------------------

-- subtract-flags reference (used by SUBI/SBCI/CPI) — reuse refSub.
scenarios :: [Scenario]
scenarios =
    [ -- SUBI R16,0x12 : Rd = 0x30-0x12 = 0x1E ; SUB flags
      let a = 0x30; k = 0x12; (res,fs) = refSub a k 0
      in Scenario "SUBI" instrSUBI [0x5102] [("GPR:16", fromIntegral a)] []
           (manyChk (chkReg "SUBI" "GPR:16" (fromIntegral res) : [chkFlags "SUBI" fs]))

    , -- SBCI R16,0x12 with carry-in set
      let a = 0x30; k = 0x12; cin = 1; (res,fs) = refSub a k cin
      in Scenario "SBCI" instrSBCI [0x4102]
           [("GPR:16", fromIntegral a), ("SREG", sregOf [("C",cin)])] []
           (manyChk (chkReg "SBCI" "GPR:16" (fromIntegral res) : [chkFlags "SBCI" fs]))

    , -- ANDI R16,0x0F : 0x3C & 0x0F = 0x0C ; logic flags (Z,N,V=0,S=N)
      let a = 0x3C; k = 0x0F; (res,fs) = refLogic (.&.) a k
      in Scenario "ANDI" instrANDI [0x700F] [("GPR:16", fromIntegral a)] []
           (manyChk (chkReg "ANDI" "GPR:16" (fromIntegral res) : [chkFlags "ANDI" fs]))

    , -- ORI R16,0xF0 : 0x0C | 0xF0 = 0xFC ; logic flags
      let a = 0x0C; k = 0xF0; (res,fs) = refLogic (.|.) a k
      in Scenario "ORI" instrORI [0x6F00] [("GPR:16", fromIntegral a)] []
           (manyChk (chkReg "ORI" "GPR:16" (fromIntegral res) : [chkFlags "ORI" fs]))

    , -- CPI R16,0x12 : compare, Rd unchanged, SUB flags of 0x30-0x12
      let a = 0x30; k = 0x12; (_res,fs) = refSub a k 0
      in Scenario "CPI" instrCPI [0x3102] [("GPR:16", fromIntegral a)] []
           (manyChk (chkReg "CPI" "GPR:16" (fromIntegral a) : [chkFlags "CPI" fs]))

    , -- LDI R16,0xAB : Rd = K, no flags
      Scenario "LDI" instrLDI [0xEA0B] [] []
           (chkReg "LDI" "GPR:16" 0xAB)

    , -- ADIW R24,0x05 : R25:R24 = 0x00FF + 5 = 0x0104 -> R24=0x04 R25=0x01
      let lo0 = 0xFF :: Integer; hi0 = 0x00 :: Integer; v0 = hi0*256 + lo0; v1 = (v0 + 5) .&. 0xFFFF
          rlo = v1 .&. 0xFF; rhi = (v1 `shiftR` 8) .&. 0xFF
          n = msb (fromIntegral rhi); z = if v1 == 0 then 1 else 0
          c = if v0 + 5 > 0xFFFF then 1 else 0
          v = if not (testBit hi0 7) && msb (fromIntegral rhi) == 1 then 1 else 0
      in Scenario "ADIW" instrADIW [0x9605]
           [("GPR:24", fromIntegral lo0), ("GPR:25", fromIntegral hi0)] []
           (manyChk [ chkReg "ADIW" "GPR:24" rlo, chkReg "ADIW" "GPR:25" rhi
                    , chkFlags "ADIW" [("Z",z),("N",n),("V",v),("S",n `xor` v),("C",c)] ])

    , -- SBIW R24,0x05 : R25:R24 = 0x0102 - 5 = 0x00FD -> R24=0xFD R25=0x00
      let lo0 = 0x02 :: Integer; hi0 = 0x01 :: Integer; v0 = hi0*256 + lo0; v1 = (v0 - 5) .&. 0xFFFF
          rlo = v1 .&. 0xFF; rhi = (v1 `shiftR` 8) .&. 0xFF
          n = msb (fromIntegral rhi); z = if v1 == 0 then 1 else 0
          c = if 5 > v0 then 1 else 0
          v = if testBit hi0 7 && msb (fromIntegral rhi) == 0 then 1 else 0
      in Scenario "SBIW" instrSBIW [0x9705]
           [("GPR:24", fromIntegral lo0), ("GPR:25", fromIntegral hi0)] []
           (manyChk [ chkReg "SBIW" "GPR:24" rlo, chkReg "SBIW" "GPR:25" rhi
                    , chkFlags "SBIW" [("Z",z),("N",n),("V",v),("S",n `xor` v),("C",c)] ])

    , -- MOVW R16,R18 : copy R19:R18 -> R17:R16, no flags
      Scenario "MOVW" instrMOVW [0x0189]
           [("GPR:18", 0xAA), ("GPR:19", 0xBB)] []
           (manyChk [ chkReg "MOVW" "GPR:16" 0xAA, chkReg "MOVW" "GPR:17" 0xBB ])

    , -- MUL R16,R17 : R1:R0 = 0x12*0x10 = 0x0120 ; C = bit15(=0), Z=0
      let a = 0x12 :: Integer; b = 0x10 :: Integer; p = a*b; r0 = p .&. 0xFF; r1 = (p `shiftR` 8) .&. 0xFF
          c = if testBit p 15 then 1 else 0
          z = if p == 0 then 1 else 0
      in Scenario "MUL" instrMUL [0x9F01]
           [("GPR:16", fromIntegral a), ("GPR:17", fromIntegral b)] []
           (manyChk [ chkReg "MUL" "GPR:0" (fromIntegral r0), chkReg "MUL" "GPR:1" (fromIntegral r1)
                    , chkFlags "MUL" [("C",c),("Z",z)] ])

    , -- MULS R16,R17 : signed 0xFF(-1) * 0x02(2) = -2 = 0xFFFE
      let a = 0xFF :: Integer; b = 0x02 :: Integer; sa = a - 256; p = (sa*b) .&. 0xFFFF
          r0 = p .&. 0xFF; r1 = (p `shiftR` 8) .&. 0xFF
          c = if testBit p 15 then 1 else 0
          z = if p == 0 then 1 else 0
      in Scenario "MULS" instrMULS [0x0201]
           [("GPR:16", fromIntegral a), ("GPR:17", fromIntegral b)] []
           (manyChk [ chkReg "MULS" "GPR:0" (fromIntegral r0), chkReg "MULS" "GPR:1" (fromIntegral r1)
                    , chkFlags "MULS" [("C",c),("Z",z)] ])

    , -- CP R16,R17 : compare 0x30 vs 0x12, SUB flags, no write
      let a = 0x30; b = 0x12; (_r,fs) = refSub a b 0
      in Scenario "CP" instrCP [0x1701]
           [("GPR:16", fromIntegral a), ("GPR:17", fromIntegral b)] []
           (manyChk (chkReg "CP" "GPR:16" (fromIntegral a) : [chkFlags "CP" fs]))

    , -- CPC R16,R17 with carry : SUB-with-carry flags
      let a = 0x30; b = 0x12; cin = 1; (_r,fs) = refSub a b cin
      in Scenario "CPC" instrCPC [0x0701]
           [("GPR:16", fromIntegral a), ("GPR:17", fromIntegral b), ("SREG", sregOf [("C",cin)])] []
           (manyChk (chkReg "CPC" "GPR:16" (fromIntegral a) : [chkFlags "CPC" fs]))

    , -- BST R16,3 : T = bit3 of 0x08 = 1
      Scenario "BST" instrBST [0xFB03] [("GPR:16", 0x08)] []
           (chkFlag "BST" "T" 1)

    , -- BLD R16,3 : bit3 of Rd = T(=1) -> 0x00 | 0x08 = 0x08
      Scenario "BLD" instrBLD [0xF903] [("GPR:16", 0x00), ("SREG", sregOf [("T",1)])] []
           (chkReg "BLD" "GPR:16" 0x08)

    , -- SBRC R16,3 : bit3 clear (0x00) -> skip next -> PC = 0+2 = 2
      Scenario "SBRC-skip" instrSBRC [0xFD03] [("GPR:16", 0x00), ("PC", 0)] []
           (chkReg "SBRC-skip" "PC" 2)

    , -- SBRC R16,3 : bit3 set (0x08) -> no skip -> PC = 0+1 = 1
      Scenario "SBRC-not" instrSBRC [0xFD03] [("GPR:16", 0x08), ("PC", 0)] []
           (chkReg "SBRC-not" "PC" 1)

    , -- SBRS R16,3 : bit3 set (0x08) -> skip next -> PC = 2
      Scenario "SBRS-skip" instrSBRS [0xFF03] [("GPR:16", 0x08), ("PC", 0)] []
           (chkReg "SBRS-skip" "PC" 2)

    , -- SBRS R16,3 : bit3 clear (0x00) -> no skip -> PC = 1
      Scenario "SBRS-not" instrSBRS [0xFF03] [("GPR:16", 0x00), ("PC", 0)] []
           (chkReg "SBRS-not" "PC" 1)

    , -- BSET 0 (SEC) : set C
      Scenario "BSET" instrBSET [0x9408] [("SREG", 0)] []
           (chkFlag "BSET" "C" 1)

    , -- BCLR 0 (CLC) : clear C
      Scenario "BCLR" instrBCLR [0x9488] [("SREG", sregOf [("C",1)])] []
           (chkFlag "BCLR" "C" 0)

    , -- BRBS 1 (BREQ) with Z set, offset .+2 (k=1) -> taken: PC = 0+1+1 = 2
      Scenario "BRBS-taken" instrBRBS [0xF009] [("PC", 0), ("SREG", sregOf [("Z",1)])] []
           (chkReg "BRBS" "PC" 2)

    , -- BRBS 1 (BREQ) with Z clear -> not taken: PC = 0+1 = 1
      Scenario "BRBS-not" instrBRBS [0xF009] [("PC", 0), ("SREG", 0)] []
           (chkReg "BRBS" "PC" 1)

    , -- BRBC 1 (BRNE) with Z clear, k=1 -> taken: PC = 2
      Scenario "BRBC-taken" instrBRBC [0xF409] [("PC", 0), ("SREG", 0)] []
           (chkReg "BRBC" "PC" 2)

    , -- BRBC 1 (BRNE) with Z set -> not taken: PC = 1
      Scenario "BRBC-not" instrBRBC [0xF409] [("PC", 0), ("SREG", sregOf [("Z",1)])] []
           (chkReg "BRBC" "PC" 1)

    , -- IN R16,0x10 : Rd = dataMem[0x10+0x20=0x30]
      Scenario "IN" instrIN [0xB300] [] [(0x30, 0x77)]
           (chkReg "IN" "GPR:16" 0x77)

    , -- OUT 0x10,R16 : dataMem[0x30] = R16
      Scenario "OUT" instrOUT [0xBB00] [("GPR:16", 0x99)] []
           (chkMem "OUT" 0x30 0x99)

    , -- SBI 0x05,3 : set bit3 of dataMem[0x05+0x20=0x25] : 0x01 -> 0x09
      Scenario "SBI" instrSBI [0x9A2B] [] [(0x25, 0x01)]
           (chkMem "SBI" 0x25 0x09)

    , -- CBI 0x05,3 : clear bit3 of dataMem[0x25] : 0x0F -> 0x07
      Scenario "CBI" instrCBI [0x982B] [] [(0x25, 0x0F)]
           (chkMem "CBI" 0x25 0x07)

    , -- SBIC 0x05,3 : bit3 clear (0x00) -> skip -> PC = 2
      Scenario "SBIC-skip" instrSBIC [0x992B] [("PC", 0)] [(0x25, 0x00)]
           (chkReg "SBIC-skip" "PC" 2)

    , -- SBIC 0x05,3 : bit3 set (0x08) -> no skip -> PC = 1
      Scenario "SBIC-not" instrSBIC [0x992B] [("PC", 0)] [(0x25, 0x08)]
           (chkReg "SBIC-not" "PC" 1)

    , -- SBIS 0x05,3 : bit3 set (0x08) -> skip -> PC = 2
      Scenario "SBIS-skip" instrSBIS [0x9B2B] [("PC", 0)] [(0x25, 0x08)]
           (chkReg "SBIS-skip" "PC" 2)

    , -- SBIS 0x05,3 : bit3 clear (0x00) -> no skip -> PC = 1
      Scenario "SBIS-not" instrSBIS [0x9B2B] [("PC", 0)] [(0x25, 0x00)]
           (chkReg "SBIS-not" "PC" 1)

    -- LD/ST X (GPR:27:26), Y (29:28), Z (31:30), pointer = 0x40
    , Scenario "LD_X" instrLD_X [0x910C] [("GPR:26",0x40),("GPR:27",0x00)] [(0x40,0x5A)]
           (manyChk [ chkReg "LD_X" "GPR:16" 0x5A, chkReg "LD_X" "GPR:26" 0x40, chkReg "LD_X" "GPR:27" 0x00 ])
    , Scenario "LD_Xplus" instrLD_Xplus [0x910D] [("GPR:26",0x40),("GPR:27",0x00)] [(0x40,0x5A)]
           (manyChk [ chkReg "LD_X+" "GPR:16" 0x5A, chkReg "LD_X+" "GPR:26" 0x41, chkReg "LD_X+" "GPR:27" 0x00 ])
    , Scenario "LD_Xminus" instrLD_Xminus [0x910E] [("GPR:26",0x40),("GPR:27",0x00)] [(0x3F,0x5A)]
           (manyChk [ chkReg "LD_-X" "GPR:16" 0x5A, chkReg "LD_-X" "GPR:26" 0x3F, chkReg "LD_-X" "GPR:27" 0x00 ])

    , Scenario "LD_Y" instrLD_Y [0x8108] [("GPR:28",0x40),("GPR:29",0x00)] [(0x40,0x5B)]
           (manyChk [ chkReg "LD_Y" "GPR:16" 0x5B, chkReg "LD_Y" "GPR:28" 0x40 ])
    , Scenario "LD_Yplus" instrLD_Yplus [0x9109] [("GPR:28",0x40),("GPR:29",0x00)] [(0x40,0x5B)]
           (manyChk [ chkReg "LD_Y+" "GPR:16" 0x5B, chkReg "LD_Y+" "GPR:28" 0x41 ])
    , Scenario "LD_Yminus" instrLD_Yminus [0x910A] [("GPR:28",0x40),("GPR:29",0x00)] [(0x3F,0x5B)]
           (manyChk [ chkReg "LD_-Y" "GPR:16" 0x5B, chkReg "LD_-Y" "GPR:28" 0x3F ])

    , Scenario "LD_Z" instrLD_Z [0x8100] [("GPR:30",0x40),("GPR:31",0x00)] [(0x40,0x5C)]
           (manyChk [ chkReg "LD_Z" "GPR:16" 0x5C, chkReg "LD_Z" "GPR:30" 0x40 ])
    , Scenario "LD_Zplus" instrLD_Zplus [0x9101] [("GPR:30",0x40),("GPR:31",0x00)] [(0x40,0x5C)]
           (manyChk [ chkReg "LD_Z+" "GPR:16" 0x5C, chkReg "LD_Z+" "GPR:30" 0x41 ])
    , Scenario "LD_Zminus" instrLD_Zminus [0x9102] [("GPR:30",0x40),("GPR:31",0x00)] [(0x3F,0x5C)]
           (manyChk [ chkReg "LD_-Z" "GPR:16" 0x5C, chkReg "LD_-Z" "GPR:30" 0x3F ])

    , Scenario "ST_X" instrST_X [0x930C] [("GPR:16",0xA1),("GPR:26",0x40),("GPR:27",0x00)] []
           (manyChk [ chkMem "ST_X" 0x40 0xA1, chkReg "ST_X" "GPR:26" 0x40 ])
    , Scenario "ST_Xplus" instrST_Xplus [0x930D] [("GPR:16",0xA1),("GPR:26",0x40),("GPR:27",0x00)] []
           (manyChk [ chkMem "ST_X+" 0x40 0xA1, chkReg "ST_X+" "GPR:26" 0x41 ])
    , Scenario "ST_Xminus" instrST_Xminus [0x930E] [("GPR:16",0xA1),("GPR:26",0x40),("GPR:27",0x00)] []
           (manyChk [ chkMem "ST_-X" 0x3F 0xA1, chkReg "ST_-X" "GPR:26" 0x3F ])

    , Scenario "ST_Y" instrST_Y [0x8308] [("GPR:16",0xA2),("GPR:28",0x40),("GPR:29",0x00)] []
           (manyChk [ chkMem "ST_Y" 0x40 0xA2, chkReg "ST_Y" "GPR:28" 0x40 ])
    , Scenario "ST_Yplus" instrST_Yplus [0x9309] [("GPR:16",0xA2),("GPR:28",0x40),("GPR:29",0x00)] []
           (manyChk [ chkMem "ST_Y+" 0x40 0xA2, chkReg "ST_Y+" "GPR:28" 0x41 ])
    , Scenario "ST_Yminus" instrST_Yminus [0x930A] [("GPR:16",0xA2),("GPR:28",0x40),("GPR:29",0x00)] []
           (manyChk [ chkMem "ST_-Y" 0x3F 0xA2, chkReg "ST_-Y" "GPR:28" 0x3F ])

    , Scenario "ST_Z" instrST_Z [0x8300] [("GPR:16",0xA3),("GPR:30",0x40),("GPR:31",0x00)] []
           (manyChk [ chkMem "ST_Z" 0x40 0xA3, chkReg "ST_Z" "GPR:30" 0x40 ])
    , Scenario "ST_Zplus" instrST_Zplus [0x9301] [("GPR:16",0xA3),("GPR:30",0x40),("GPR:31",0x00)] []
           (manyChk [ chkMem "ST_Z+" 0x40 0xA3, chkReg "ST_Z+" "GPR:30" 0x41 ])
    , Scenario "ST_Zminus" instrST_Zminus [0x9302] [("GPR:16",0xA3),("GPR:30",0x40),("GPR:31",0x00)] []
           (manyChk [ chkMem "ST_-Z" 0x3F 0xA3, chkReg "ST_-Z" "GPR:30" 0x3F ])

    , -- LDS R16,0x200 : Rd = dataMem[0x200]; PC advances by 2
      Scenario "LDS" instrLDS [0x9100, 0x0200] [("PC",0)] [(0x200, 0x6E)]
           (manyChk [ chkReg "LDS" "GPR:16" 0x6E, chkReg "LDS" "PC" 2 ])

    , -- STS 0x200,R16 : dataMem[0x200] = R16; PC advances by 2
      Scenario "STS" instrSTS [0x9300, 0x0200] [("GPR:16",0x77),("PC",0)] []
           (manyChk [ chkMem "STS" 0x200 0x77, chkReg "STS" "PC" 2 ])

    , -- PUSH R16 : dataMem[SP] = R16 ; SP -= 1
      Scenario "PUSH" instrPUSH [0x930F] [("GPR:16",0x55),("SP",0x100)] []
           (manyChk [ chkMem "PUSH" 0x100 0x55, chkReg "PUSH" "SP" 0xFF ])

    , -- POP R16 : SP += 1 ; R16 = dataMem[SP]
      Scenario "POP" instrPOP [0x910F] [("SP",0xFF)] [(0x100, 0x66)]
           (manyChk [ chkReg "POP" "GPR:16" 0x66, chkReg "POP" "SP" 0x100 ])

    , -- RJMP .+2 (k=0) : PC = PC+1+0 = 1
      Scenario "RJMP" instrRJMP [0xC000] [("PC",0)] []
           (chkReg "RJMP" "PC" 1)

    , -- RCALL .+2 (k=0) : PC = (PC+1)+0 = 1 ; push return addr (PC+1=1), SP-=2
      Scenario "RCALL" instrRCALL [0xD000] [("PC",0),("SP",0x100)] []
           (manyChk [ chkReg "RCALL" "PC" 1, chkReg "RCALL" "SP" 0xFE
                    , chkMem "RCALL" 0x100 0x00, chkMem "RCALL" 0xFF 0x01 ])

    , -- RET : pop PC from stack (lo at SP+1, hi at SP+2) ; SP += 2
      Scenario "RET" instrRET [0x9508] [("SP",0xFE)] [(0xFF, 0x34), (0x100, 0x12)]
           (manyChk [ chkReg "RET" "PC" 0x1234, chkReg "RET" "SP" 0x100 ])

    , -- RETI : same as RET plus set I
      Scenario "RETI" instrRETI [0x9518] [("SP",0xFE)] [(0xFF, 0x34), (0x100, 0x12)]
           (manyChk [ chkReg "RETI" "PC" 0x1234, chkReg "RETI" "SP" 0x100, chkFlag "RETI" "I" 1 ])

    , -- ICALL : PC = Z (R31:R30) ; push return addr, SP -= 2
      Scenario "ICALL" instrICALL [0x9509] [("PC",0),("SP",0x100),("GPR:30",0x50),("GPR:31",0x00)] []
           (manyChk [ chkReg "ICALL" "PC" 0x50, chkReg "ICALL" "SP" 0xFE ])

    , -- IJMP : PC = Z
      Scenario "IJMP" instrIJMP [0x9409] [("PC",0),("GPR:30",0x50),("GPR:31",0x00)] []
           (chkReg "IJMP" "PC" 0x50)

    , -- CALL 0x100 : PC = 2nd word (0x0080) ; push return addr (p+2)
      Scenario "CALL" instrCALL [0x940E, 0x0080] [("PC",0),("SP",0x100)] []
           (manyChk [ chkReg "CALL" "PC" 0x80, chkReg "CALL" "SP" 0xFE ])

    , -- JMP 0x100 : PC = 2nd word (0x0080)
      Scenario "JMP" instrJMP [0x940C, 0x0080] [("PC",0)] []
           (chkReg "JMP" "PC" 0x80)

    , -- CPSE R16,R17 equal -> skip next -> PC = 2
      Scenario "CPSE-skip" instrCPSE [0x1301] [("GPR:16",0x30),("GPR:17",0x30),("PC",0)] []
           (chkReg "CPSE-skip" "PC" 2)

    , -- CPSE R16,R17 unequal -> no skip -> PC = 1
      Scenario "CPSE-not" instrCPSE [0x1301] [("GPR:16",0x30),("GPR:17",0x31),("PC",0)] []
           (chkReg "CPSE-not" "PC" 1)

    , -- NOP : PC += 1, nothing else
      Scenario "NOP" instrNOP [0x0000] [("PC",0)] []
           (chkReg "NOP" "PC" 1)
    ]

-- Distinct instruction names covered by scenarios (some appear in >1 scenario).
scenarioInstrs :: [String]
scenarioInstrs =
    [ "SUBI","SBCI","ANDI","ORI","CPI","LDI","ADIW","SBIW","MOVW","MUL","MULS"
    , "CP","CPC","CPSE","BST","BLD","SBRC","SBRS","BSET","BCLR","BRBS","BRBC"
    , "IN","OUT","SBI","CBI","SBIC","SBIS"
    , "LD_X","LD_Xplus","LD_Xminus","LD_Y","LD_Yplus","LD_Yminus","LD_Z","LD_Zplus","LD_Zminus"
    , "ST_X","ST_Xplus","ST_Xminus","ST_Y","ST_Yplus","ST_Yminus","ST_Z","ST_Zplus","ST_Zminus"
    , "LDS","STS","PUSH","POP","RJMP","RCALL","RET","RETI","ICALL","IJMP","CALL","JMP","NOP"
    ]

runScenarioChecks :: Scenario -> [(String, Bool, String)]
runScenarioChecks sc = scChecks sc (runScenario sc)

main :: IO ()
main = do
    let refResults = concatMap (\s -> concatMap (checkSample s) (samplesFor s)) specs
        samplesFor (Spec _ _ _ k) = case k of
            Un _   -> map (\a -> (a,0,0)) samplesUn
            UnC _  -> concatMap (\a -> [(a,0,0),(a,0,1)]) samplesUn
            _      -> samplesBin
        scResults = concatMap runScenarioChecks scenarios
        results   = refResults ++ scResults
        fails = [ (l,d) | (l,ok,d) <- results, not ok ]
        nTot  = length results
        -- instruction coverage: 16 reference specs + distinct scenario instrs
        nInstrs = length specs + length scenarioInstrs
    putStrLn "== AVR per-instruction state+flag regression (Sim vs reference model) =="
    mapM_ (\(l,d) -> putStrLn ("  FAIL: " ++ l ++ "  (" ++ d ++ ")")) fails
    putStrLn $ "== instructions covered: " ++ show nInstrs
                 ++ " (16 reference-model + " ++ show (length scenarioInstrs)
                 ++ " scenario instructions; every distinct core instruction body) =="
    putStrLn $ "-- " ++ show (nTot - length fails) ++ "/" ++ show nTot ++ " checks pass; "
                     ++ show (length fails) ++ " tracked failures --"
    if null fails then exitSuccess else exitFailure
