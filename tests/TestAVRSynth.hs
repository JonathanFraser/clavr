{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import System.Directory (createDirectoryIfMissing)

import Hdl.Sig
import Hdl.Net (DomId(..), ClockEdge(..), ResetPolarity(..), execDesign, NetM, freshWire, emit, NetNode(..))
import Hdl.Emit.Vhdl
import Isacle.ISA.Backend.SynthCPU (synthHarvardCPU', CpuMemIface(..))

import AVR.ISA (avrCPUDef, avrATmegaISA)
import AVR.ISA.Types (AvrCore)

data Sys

instance KnownDom Sys where
    domId _ = DomId "sys" 16000000 Rising ActiveHigh "rst"

-- | Standalone top-level entity for the abstract 'synthHarvardCPU''.
avrTop :: NetM ()
avrTop = do
    let domInfo = domId (Proxy @Sys)
        cw = fromIntegral (natVal (Proxy @16)) :: Int
        inPort :: forall a. String -> Int -> NetM (Sig Sys a)
        inPort name bits = do { wid <- freshWire; emit (NInput wid name bits domInfo); pure (SWire wid) }
        outPort :: forall a. String -> Int -> Sig Sys a -> NetM ()
        outPort name bits sig = do { wid <- materialize sig; emit (NOutput wid name bits domInfo) }
    codeD  <- inPort "code_rd_data" cw   -- single code read port
    dmemRd <- inPort "data_rd_data" 8
    stall  <- inPort "stall"        1
    irqP   <- inPort "irq_pending"  1
    irqV   <- inPort "irq_vector"   cw
    cmi <- synthHarvardCPU' @(AvrCore 16) @Sig @NetM @Sys @8 @16 @16 @16
               avrCPUDef avrATmegaISA codeD dmemRd stall irqP irqV
    outPort "code_rd_addr" (cmiCodeAddrW cmi) (cmiCodeRdAddr cmi)
    outPort "data_rd_addr" (cmiDataAddrW cmi) (cmiDataRdAddr cmi)
    outPort "data_wr_en"   1                  (cmiDataWrEn   cmi)
    outPort "data_wr_addr" (cmiDataAddrW cmi) (cmiDataWrAddr cmi)
    outPort "data_wr_data" (cmiWordW     cmi) (cmiDataWrData cmi)

main :: IO ()
main = do
    let outDir = "build/avr_cpu"
    createDirectoryIfMissing True outDir
    let design = execDesign "avr_cpu" avrTop
    emitVhdlDesignFiles outDir design
    putStrLn $ "AVR synthesis done — VHDL written to " ++ outDir
