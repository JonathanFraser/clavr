{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass   #-}
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

import GHC.Generics (Generic)

import Hdl.Sig
import Hdl.Net (DomId(..), ClockEdge(..), ResetPolarity(..))
import Hdl.Types (Named)
import Hdl.Prim (Unsigned)
import Hdl.Emit.Vhdl
import Isacle.System.SystemDSL
import Isacle.System.HdlCircuit (GpioPhys(..), TimerPhys(..))

import AVR.ISA (avrChip)
import AVR.Program (readAvrProgram)

data Dom10MHz
instance KnownDom Dom10MHz where
    domId _ = DomId "dom10mhz" 10000000 Rising ActiveHigh "rst"

data AvrIn = AvrIn
    { uart_rx :: Sig Dom10MHz Bool
    , gpio_in :: Sig Dom10MHz (Unsigned 8)
    } deriving (Generic, Named)

data AvrOut = AvrOut
    { gpio_port  :: Sig Dom10MHz (Unsigned 8)
    , gpio_ddr   :: Sig Dom10MHz (Unsigned 8)
    , gpiot_port :: Sig Dom10MHz (Unsigned 8)
    } deriving (Generic, Named)

avrIrqSoc :: SysDSL m => [Integer] -> AvrIn -> m AvrOut
avrIrqSoc romWords AvrIn{ uart_rx = uartRx, gpio_in = gpioIn } = do
    uart0  <- createUart  "uart0"  uartRx
    timer0 <- createTimer "timer0" sigTrue     -- tick every cycle → overflow at 256
    gpio  <- createGpio  "gpio"  gpioIn
    gpiot  <- createGpio  "gpiot"  0
    ramp0  <- createRamp  "ramp0"  sigTrue
    ram0   <- createRam   2048 [] "ram0"

    (dataBus, (timerOut, gpioOut, gpiotOut)) <- createBus @_ @SimpleBus "databus" $ do
        _         <- attachPeripheral 0x0040 uart0
        timerOut' <- attachPeripheral 0x0050 timer0
        gpioOut'  <- attachPeripheral 0x0060 gpio
        gpiotOut' <- attachPeripheral 0x0020 gpiot
        _         <- attachPeripheral 0x0070 ramp0
        _         <- attachPeripheral 0x0200 ram0
        return (timerOut', gpioOut', gpiotOut')

    coderom <- createRom 65536 (RomImage romWords :: RomImage (Unsigned 16)) "coderom"
    (codeBus, ()) <- createBus @_ @SimpleBus "codebus" $ do
        _ <- attachPeripheral 0x0 coderom
        return ()

    -- The timer overflow is the interrupt source; createIrq latches it (held
    -- across the handler, cleared by irq_ack) and vectors the CPU to word 0x0020.
    irq <- createIrq sigTrue [(timerOvfIrq timerOut, 0x0020)]
    createHarvardCPU "cpu" avrChip codeBus dataBus irq

    pure AvrOut { gpio_port  = gpioPort gpioOut
                , gpio_ddr   = gpioDdr  gpioOut
                , gpiot_port = gpioPort gpiotOut }

main :: IO ()
main = do
    args <- getArgs
    let progBin = case args of { (p:_)   -> p; _ -> "tests/fixtures/interrupt_test.bin" }
        outDir  = case args of { (_:o:_) -> o; _ -> "build/avr_irq_soc" }
    romWords <- readAvrProgram progBin
    createDirectoryIfMissing True outDir
    let design = execSystemDSL "avr_soc" (avrIrqSoc romWords)
    emitVhdlDesignFiles outDir design
    putStrLn $ "AVR IRQ SoC synthesis done — " ++ progBin ++ " → " ++ outDir
