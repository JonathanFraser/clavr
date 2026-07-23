{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass   #-}
-- | AVR SoC that instantiates a __hand-written__ Wishbone slave.
--
-- Demonstrates the interoperability path: a slave authored independently in VHDL
-- (@tests/ghdl/my_wb_slave.vhd@), written to the Wishbone B4 classic signalling
-- contract, is dropped into the system with the two orthogonal primitives —
-- 'externEntity' (general black-box instantiation: clock/reset + all its ports)
-- and 'attachSlave' (the bus facet only: address window + ACK handshake).
--
-- Memory map (Wishbone data bus):
--   0x0060 – 0x0062   GPIO (PIN / DDR / PORT)
--   0x0090 – 0x009F   my_wb_slave (hand-written): reads 0x3B after 4 wait states
module Main where

import Prelude
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)

import GHC.Generics (Generic)

import Hdl.Sig
import Hdl.Net (DomId(..), ClockEdge(..), ResetPolarity(..))
import Hdl.Types (Named)
import Hdl.Prim (Unsigned)
import Hdl.Emit.Vhdl
import Isacle.System.SystemDSL
import Isacle.System.HdlCircuit (GpioPhys(..))

import AVR.ISA (avrChip)
import AVR.Program (readAvrProgram)

data Dom10MHz
instance KnownDom Dom10MHz where
    domId _ = DomId "dom10mhz" 10000000 Rising ActiveHigh "rst"

data AvrIn = AvrIn
    { gpio_in :: Sig Dom10MHz (Unsigned 8)
    } deriving (Generic, Named)

data AvrOut = AvrOut
    { gpio_port :: Sig Dom10MHz (Unsigned 8)
    , gpio_ddr  :: Sig Dom10MHz (Unsigned 8)
    } deriving (Generic, Named)

wbExternSoc :: [Integer] -> AvrIn -> SysNet AvrOut
wbExternSoc romWords AvrIn{ gpio_in = gpioIn } = do
    gpio0 <- createGpio "gpio" gpioIn

    -- Data bus is Wishbone.  GPIO is a normal generated peripheral; the
    -- hand-written slave is attached via its raw Wishbone port.
    (dataBus, (gpioOut, wbm, sink)) <- createBus @_ @Wishbone "databus" $ do
        g            <- attachPeripheral 0x0060 gpio0
        (wbm, sink)  <- attachSlave "my_wb_slave" 0x0090 0x0010
        return (g, wbm, sink)

    -- Instantiate the hand-authored Wishbone slave as a black box, driven by the
    -- bus master signals, then tie its response back into the bus.  A general
    -- block with extra I/O would simply add more fields here — the bus never sees
    -- them.
    resp <- externEntity "my_wb_slave"
                WbSlaveIn { wb_cyc_i = wbmCyc wbm
                          , wb_stb_i = wbmStb wbm
                          , wb_we_i  = wbmWe  wbm
                          , wb_adr_i = wbmAdr wbm
                          , wb_dat_i = wbmDat wbm }
    connectWbSlaveOut sink resp

    coderom <- createRom 65536 (RomImage romWords :: RomImage (Unsigned 16)) "coderom"
    (codeBus, ()) <- createBus @_ @SimpleBus "codebus" $ do
        _ <- attachPeripheral 0x0 coderom
        return ()

    dormant <- noIrq
    createHarvardCPU "cpu" avrChip codeBus dataBus dormant

    pure AvrOut { gpio_port = gpioPort gpioOut
                , gpio_ddr  = gpioDdr  gpioOut }

main :: IO ()
main = do
    args <- getArgs
    let progBin = case args of { (p:_)   -> p; _ -> "tests/fixtures/wb_extern.bin" }
        outDir  = case args of { (_:o:_) -> o; _ -> "build/wb_extern_soc" }
    romWords <- readAvrProgram progBin
    createDirectoryIfMissing True outDir
    let design = execSystemDSL "avr_soc" (wbExternSoc romWords)
    emitVhdlDesignFiles outDir design
    putStrLn $ "AVR Wishbone-extern SoC synthesis done — " ++ progBin ++ " → " ++ outDir
