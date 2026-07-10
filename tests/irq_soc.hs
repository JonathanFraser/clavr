{-# LANGUAGE TypeApplications #-}
-- | AVR interrupt-demo SoC — identical peripheral set to the coverage SoC, but
-- the timer's tick is enabled (so it overflows) and its overflow drives a
-- 'createIrq' latching interrupt controller into the CPU (vector 0x0020).  Used
-- by the @test_irq@ GHDL case to validate the interrupt path end-to-end:
-- timer overflow → createIrq (latched, cleared by irq_ack) → CPU vectors to the
-- ISR → GPIO PORT = 0x55.
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
import Isacle.System.HdlCircuit (GpioPhys(..), TimerPhys(..))

import AVR.ISA (avrChip)

data Dom10MHz
instance KnownDom Dom10MHz where
    domId _ = DomId "dom10mhz" 10000000 Rising ActiveHigh "rst"

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
    parseLE (lo:hi:t) = ((fromIntegral hi `shiftL` 8) .|. fromIntegral lo) : parseLE t
    nextPow2 k | k <= 1 = 1 | otherwise = 2 * nextPow2 ((k + 1) `div` 2)

avrIrqSoc :: [Integer] -> SysDSL ()
avrIrqSoc romWords = do
    uartRx  <- sysInput "uart_rx"  :: SysDSL (Sig Dom10MHz Bool)
    gpioAIn <- sysInput "gpio_a_in" :: SysDSL (Sig Dom10MHz (Unsigned 8))
    uart0  <- createUart  "uart0"  uartRx
    timer0 <- createTimer "timer0" sigTrue     -- tick every cycle → overflow at 256
    gpio0  <- createGpio  "gpio0"  gpioAIn
    gpiot  <- createGpio  "gpiot"  0
    ramp0  <- createRamp  "ramp0"  sigTrue
    ram0   <- createRam   2048 [] "ram0"

    (dataBus, (timerOut, gpioOut, gpiotOut)) <- createBus @SimpleBus "databus" $ do
        _         <- attachPeripheral 0x0040 uart0
        timerOut' <- attachPeripheral 0x0050 timer0
        gpioOut'  <- attachPeripheral 0x0060 gpio0
        gpiotOut' <- attachPeripheral 0x0020 gpiot
        _         <- attachPeripheral 0x0070 ramp0
        _         <- attachPeripheral 0x0200 ram0
        return (timerOut', gpioOut', gpiotOut')

    coderom <- createRom 65536 (RomImage romWords :: RomImage (Unsigned 16)) "coderom"
    (codeBus, ()) <- createBus @SimpleBus "codebus" $ do
        _ <- attachPeripheral 0x0 coderom
        return ()

    -- The timer overflow is the interrupt source; createIrq latches it (held
    -- across the handler, cleared by irq_ack) and vectors the CPU to word 0x0020.
    irq <- createIrq sigTrue [(timerOvfIrq timerOut, 0x0020)]
    createHarvardCPU "cpu" avrChip codeBus dataBus irq

    sysOutput "gpio_port"  (gpioPort gpioOut)
    sysOutput "gpio_ddr"   (gpioDdr  gpioOut)
    sysOutput "gpiot_port" (gpioPort gpiotOut)

main :: IO ()
main = do
    args <- getArgs
    let progBin = case args of { (p:_)   -> p; _ -> "tests/fixtures/interrupt_test.bin" }
        outDir  = case args of { (_:o:_) -> o; _ -> "build/avr_irq_soc" }
    romWords <- readBin16LE progBin
    createDirectoryIfMissing True outDir
    let design = execSystemDSL "avr_soc" (avrIrqSoc romWords)
    emitVhdlDesignFiles outDir design
    putStrLn $ "AVR IRQ SoC synthesis done — " ++ progBin ++ " → " ++ outDir
