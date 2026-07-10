{-# LANGUAGE TypeApplications #-}
-- | AVR SoC description — program ROM contents, peripheral wiring, memory map.
--
-- Memory map (data bus):
--   0x0040 – 0x0042   UART  (UDR / USR / UBRR)
--   0x0050 – 0x0052   Timer (TCCR / TCNT / OCR)
--   0x0060 – 0x0062   GPIO Port A (PIN / DDR / PORT)
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
import qualified Data.ByteString as BS
import Data.Bits (shiftL, (.|.))

import Hdl.Types
import Hdl.Net (DomId(..), ClockEdge(..), ResetPolarity(..))
import Hdl.Prim (Unsigned)
import Hdl.Emit.Vhdl
import Isacle.System.SystemDSL
import Isacle.System.HdlCircuit (GpioPhys(..), UartPhys(..))

import AVR.ISA (avrChip)

-- ---------------------------------------------------------------------------
-- Clock domain
-- ---------------------------------------------------------------------------

data Dom10MHz
instance KnownDom Dom10MHz where
    domId _ = DomId "dom10mhz" 10000000 Rising ActiveHigh "rst"

-- ---------------------------------------------------------------------------
-- Runtime binary loading
-- ---------------------------------------------------------------------------

readBin16LE :: FilePath -> IO [Integer]
readBin16LE path = do
    bytes <- BS.unpack <$> BS.readFile path
    let words16 = parseLE bytes
        n       = length words16
        n'      = nextPow2 (max 1 n)
    return (words16 ++ replicate (n' - n) 0)
  where
    parseLE []        = []
    parseLE [_]       = []
    parseLE (lo:hi:t) = ((fromIntegral hi `shiftL` 8) .|. fromIntegral lo)
                        : parseLE t
    nextPow2 k
        | k <= 1    = 1
        | otherwise = 2 * nextPow2 ((k + 1) `div` 2)

-- ---------------------------------------------------------------------------
-- SoC description (ROM contents passed in at synthesis time)
-- ---------------------------------------------------------------------------

avrSocWith :: [Integer] -> SysDSL ()
avrSocWith romWords = do
    -- Top-level inputs: UART RX line and GPIO Port A pin inputs.
    uartRx  <- sysInput "uart_rx"  :: SysDSL (Sig Dom10MHz Bool)
    gpioAIn <- sysInput "gpio_a_in" :: SysDSL (Sig Dom10MHz (Unsigned 8))

    uart0  <- createUart  "uart0"  uartRx
    timer0 <- createTimer "timer0" sigFalse
    gpio0  <- createGpio  "gpio0"  gpioAIn
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
    -- (SimpleBus) master logic for it.
    (dataBus, (uartOut, gpioOut, gpiotOut)) <- createBus @SimpleBus "databus" $ do
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
    (codeBus, ()) <- createBus @SimpleBus "codebus" $ do
        _ <- attachPeripheral 0x0 coderom
        return ()

    -- No live interrupt here: the coverage programs run with interrupts enabled
    -- at points (SEI), so a firing source would vector them away.  'noIrq' leaves
    -- the CPU's (exposed) irq inputs dormant.  To wire one, build a driver and
    -- hand it in instead:  irq <- createIrq enable [(uartRxIrq uartOut, 0x000B)]
    dormant <- noIrq
    createHarvardCPU "cpu" avrChip codeBus dataBus dormant

    _ <- pure (uartRxIrq uartOut)   -- keep the UART rxIrq output referenced

    sysOutput "gpio_port"  (gpioPort gpioOut)
    sysOutput "gpio_ddr"   (gpioDdr  gpioOut)
    sysOutput "gpiot_port" (gpioPort gpiotOut)

-- ---------------------------------------------------------------------------
-- Main: emit VHDL
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    let progBin = case args of { (p:_)   -> p; _ -> "example/Example/program.bin" }
        outDir  = case args of { (_:o:_) -> o; _ -> "build/avr_soc" }
    romWords <- readBin16LE progBin
    createDirectoryIfMissing True outDir
    let design = execSystemDSL "avr_soc" (avrSocWith romWords)
    emitVhdlDesignFiles outDir design
    putStrLn $ "AVR SoC synthesis done — " ++ progBin ++ " → " ++ outDir
