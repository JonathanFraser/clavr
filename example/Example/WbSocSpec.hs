{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass   #-}
-- | AVR SoC on a __Wishbone (stalling)__ data bus, exercising the wait-state
-- handshake end-to-end with a real CPU.
--
-- Memory map (data bus):
--   0x0060 – 0x0062   GPIO (PIN / DDR / PORT)
--   0x0080 – 0x008F   Wishbone /sub-bus/ (bus-to-bus) window
--     0x0080          Wait-state slave (reads hold the master for N cycles)
--
-- Code bus:
--   0x0000 – …        Code ROM (16-bit words, AVR word-addressed)
--
-- The wait-state slave asserts @stall@ for N cycles on a read, then returns N.
-- A master that ignored stall would sample the early (0) value; the CPU holding
-- through the stall is exactly what makes the program's @GPIO_PORT = N@ result a
-- discriminating proof.  The data bus is 'Wishbone' and the wait-state slave
-- sits on a nested 'Wishbone' sub-bus reached via 'attachBus' — so this one SoC
-- exercises both new bus features (stalling + bus-to-bus) with the AVR core.
--
-- Usage:
--   avr-wb-soc-synth [<program.bin> [<outdir>]]
--   Defaults: tests/fixtures/wb_wait.bin  build/wb_soc
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
import Isacle.System.SystemDSL hiding (SysDSL)
import Isacle.System.HdlCircuit (GpioPhys(..))

import AVR.ISA (avrChip)
import AVR.Program (readAvrProgram)

-- ---------------------------------------------------------------------------
-- Clock domain
-- ---------------------------------------------------------------------------

data Dom10MHz
instance KnownDom Dom10MHz where
    domId _ = DomId "dom10mhz" 10000000 Rising ActiveHigh "rst"

-- ---------------------------------------------------------------------------
-- Top-level pins
-- ---------------------------------------------------------------------------

data AvrIn = AvrIn
    { gpio_in :: Sig Dom10MHz (Unsigned 8)
    } deriving (Generic, Named)

data AvrOut = AvrOut
    { gpio_port :: Sig Dom10MHz (Unsigned 8)
    , gpio_ddr  :: Sig Dom10MHz (Unsigned 8)
    } deriving (Generic, Named)

-- ---------------------------------------------------------------------------
-- SoC description
-- ---------------------------------------------------------------------------

wbSocWith :: [Integer] -> AvrIn -> SysNet AvrOut
wbSocWith romWords AvrIn{ gpio_in = gpioIn } = do
    gpio0  <- createGpio        "gpio"   gpioIn
    waitst <- createWaitStates 5         "waitst"   -- stalling leaf: 5 wait states, reads 5

    -- The wait-state slave lives on its own Wishbone sub-bus — bus-to-bus.
    (subBus, ()) <- createBus @_ @Wishbone "subbus" $ do
        _ <- attachPeripheral 0x0080 waitst
        return ()

    -- Data bus is Wishbone (stall-capable): GPIO directly, plus the stalling
    -- sub-bus nested at 0x0080.  A non-stalling GPIO under a stalling parent is
    -- fine; the stalling sub-bus is legal only because the parent is stalling.
    (dataBus, gpioOut) <- createBus @_ @Wishbone "databus" $ do
        g <- attachPeripheral 0x0060 gpio0
        attachBus 0x0080 0x0010 subBus
        return g

    coderom <- createRom 65536 (RomImage romWords :: RomImage (Unsigned 16)) "coderom"
    (codeBus, ()) <- createBus @_ @SimpleBus "codebus" $ do
        _ <- attachPeripheral 0x0 coderom
        return ()

    dormant <- noIrq
    createHarvardCPU "cpu" avrChip codeBus dataBus dormant

    pure AvrOut { gpio_port = gpioPort gpioOut
                , gpio_ddr  = gpioDdr  gpioOut }

-- ---------------------------------------------------------------------------
-- Main: emit VHDL
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    let progBin = case args of { (p:_)   -> p; _ -> "tests/fixtures/wb_wait.bin" }
        outDir  = case args of { (_:o:_) -> o; _ -> "build/wb_soc" }
    romWords <- readAvrProgram progBin
    createDirectoryIfMissing True outDir
    let design = execSystemDSL "avr_soc" (wbSocWith romWords)
    emitVhdlDesignFiles outDir design
    putStrLn $ "AVR Wishbone SoC synthesis done — " ++ progBin ++ " → " ++ outDir
