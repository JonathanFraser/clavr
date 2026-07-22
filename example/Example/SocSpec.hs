{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass   #-}
-- | AVR SoC description — program ROM contents, peripheral wiring, memory map.
--
-- Memory map (data bus):
--   0x0040 – 0x0042   UART  (UDR / USR / UBRR)
--   0x0050 – 0x0052   Timer (TCCR / TCNT / OCR)
--   0x0060 – 0x0062   GPIO (PIN / DDR / PORT)
--   0x0070 – 0x0072   Ramp  (SETPOINT / STEP / CURRENT, signed datapath)
--   0x0200 – 0x09FF   2 KB SRAM
--
-- Code bus:
--   0x0000 – 0x1FFF   Code ROM (16-bit words, AVR word-addressed)
--
-- Interrupt vectors (AVR word addresses):
--   0x000B  USART RX complete
--
-- Usage:
--   avr-soc-synth [<program.bin> [<outdir>]]
--   Defaults: example/Example/program.bin  build/avr_soc
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
import Isacle.System.HdlCircuit (GpioPhys(..), UartPhys(..))

import AVR.ISA (avrChip)
import AVR.Program (readAvrProgram)

-- ---------------------------------------------------------------------------
-- Clock domain
-- ---------------------------------------------------------------------------

data Dom10MHz
instance KnownDom Dom10MHz where
    domId _ = DomId "dom10mhz" 10000000 Rising ActiveHigh "rst"

-- ---------------------------------------------------------------------------
-- SoC description (ROM contents passed in at synthesis time)
-- ---------------------------------------------------------------------------

-- | Top-level __input__ pins of the SoC: the UART RX line and the GPIO's 8 pin
-- inputs.  A plain record whose field names /are/ the pin names (that is all
-- @deriving Named@ means for an entity bundle).  The entity flow
-- ('Hdl.Entity.elaborateTop', via 'execSystemDSL') allocates one @NInput@ per
-- field and hands the record to 'avrSocWith' — no @sysInput@ needed.
data AvrIn = AvrIn
    { uart_rx :: Sig Dom10MHz Bool
    , gpio_in :: Sig Dom10MHz (Unsigned 8)
    } deriving (Generic, Named)

-- | Top-level __output__ pins: the GPIO's driven value + direction, and the
-- test GPIO's PORT.  Returned from 'avrSocWith'; the entity flow drives one
-- @NOutput@ per field — no @sysOutput@ needed.
data AvrOut = AvrOut
    { gpio_port  :: Sig Dom10MHz (Unsigned 8)
    , gpio_ddr   :: Sig Dom10MHz (Unsigned 8)
    , gpiot_port :: Sig Dom10MHz (Unsigned 8)
    } deriving (Generic, Named)

-- | The SoC is a top-level __entity__: @'AvrIn' -> m 'AvrOut'@.  Written against
-- the abstract 'SysDSL' surface (any interpreter @m@), so the description carries
-- no @NetM@/@Sig@-backend detail — only the system logic.  Top-level pins live in
-- the 'AvrIn'/'AvrOut' records and are bound by the ordinary entity-elaboration path.
avrSocWith :: SysDSL m => [Integer] -> AvrIn -> m AvrOut
avrSocWith romWords AvrIn{ uart_rx = uartRx, gpio_in = gpioIn } = do
    uart0  <- createUart  "uart0"  uartRx
    timer0 <- createTimer "timer0" sigFalse
    gpio0  <- createGpio  "gpio"   gpioIn
    -- Bit-addressable test GPIO mapped into the SBI/CBI/SBIC/SBIS I/O window
    -- (data 0x20–0x22 → I/O 0x00–0x02): its PORT (data 0x22 = I/O 0x02) is the
    -- only bit-addressable register a program can reach with SBI/CBI/SBIC/SBIS,
    -- since every real peripheral sits above I/O 0x1F.  Used by the GHDL
    -- instruction-coverage tests; harmless to the demo (an unused extra port).
    gpiot  <- createGpio  "gpiot"  0
    ramp0  <- createRamp  "ramp0"  sigTrue   -- advance every cycle (demonstrator)
    ram0   <- createRam   2048 [] "ram0"

    -- Assemble the data bus (peripherals laid out into an address map), then hand
    -- the resulting peripheral handle to the CPU — the CPU generates the matching
    -- (SimpleBus) master logic for it.  Each 'attachPeripheral' returns the
    -- peripheral's physical outputs; the bus block hands back the ones this SoC
    -- exposes as pins (below, via 'AvrOut').
    (dataBus, (uartOut, gpioOut, gpiotOut)) <- createBus @_ @SimpleBus "databus" $ do
        uartOut'  <- attachPeripheral 0x0040 uart0
        _         <- attachPeripheral 0x0050 timer0
        gpioOut'  <- attachPeripheral 0x0060 gpio0
        gpiotOut' <- attachPeripheral 0x0020 gpiot
        _         <- attachPeripheral 0x0070 ramp0
        _         <- attachPeripheral 0x0200 ram0
        return (uartOut', gpioOut', gpiotOut')

    -- The code memory is just a bus too: a combinational 16-bit-word ROM (the AVR
    -- code space, 2^16 words) assembled into its own bus, handed to the CPU
    -- alongside the data bus.
    coderom <- createRom 65536 (RomImage romWords :: RomImage (Unsigned 16)) "coderom"
    (codeBus, ()) <- createBus @_ @SimpleBus "codebus" $ do
        _ <- attachPeripheral 0x0 coderom
        return ()

    -- No live interrupt here: the coverage programs run with interrupts enabled
    -- at points (SEI), so a firing source would vector them away.  'noIrq' leaves
    -- the CPU's (exposed) irq inputs dormant.  To wire one, build a driver and
    -- hand it in instead:  irq <- createIrq enable [(uartRxIrq uartOut, 0x000B)]
    dormant <- noIrq
    createHarvardCPU "cpu" avrChip codeBus dataBus dormant

    _ <- pure (uartRxIrq uartOut)   -- keep the UART rxIrq output referenced

    -- Top-level outputs: thread the peripheral physical outputs we expose as pins
    -- into the 'AvrOut' record; the entity flow emits an NOutput per field.  The
    -- UART/timer outputs are left internal (not exposed as pins) here.
    pure AvrOut { gpio_port  = gpioPort gpioOut
                , gpio_ddr   = gpioDdr  gpioOut
                , gpiot_port = gpioPort gpiotOut }

-- ---------------------------------------------------------------------------
-- Main: emit VHDL
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    let progBin = case args of { (p:_)   -> p; _ -> "example/Example/program.bin" }
        outDir  = case args of { (_:o:_) -> o; _ -> "build/avr_soc" }
    romWords <- readAvrProgram progBin
    createDirectoryIfMissing True outDir
    let design = execSystemDSL "avr_soc" (avrSocWith romWords)
    emitVhdlDesignFiles outDir design
    putStrLn $ "AVR SoC synthesis done — " ++ progBin ++ " → " ++ outDir
