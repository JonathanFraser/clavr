-- vhdl_sim.hs — cabal test-suite entry point for all GHDL simulation tests.
--
-- Each TestCase runs avr-soc-synth with a specific binary, then simulates the
-- resulting VHDL with the matching testbench.  The testbench reports a line of
-- the form:
--
--   gpio_port = 0x<decimal>  gpio_ddr = 0x<decimal>
--
-- This runner searches for expected tokens in that output.
--
-- Requirements: ghdl, avr-as, avr-ld, avr-objcopy must be on PATH.
import Prelude
import Data.List          (isInfixOf)
import System.Exit        (ExitCode(..), exitFailure, exitSuccess)
import System.Process     (readProcessWithExitCode)
import System.IO          (hPutStrLn, stderr)

-- ---------------------------------------------------------------------------
-- Test case description
-- ---------------------------------------------------------------------------

data TestCase = TestCase
    { tcName     :: String    -- short name / build dir prefix
    , tcProgBin  :: FilePath  -- pre-assembled flat binary
    , tcTbVhd    :: FilePath  -- VHDL testbench file
    , tcStopNs   :: Int       -- simulation stop time in ns
    , tcExpected :: [(String, String)]
        -- ^ list of (signal-name, expected-decimal) tokens to find in output
        --   e.g. ("gpio_port", "0x186") means the TB must print "gpio_port = 0x186"
    }

-- ---------------------------------------------------------------------------
-- All test scenarios
-- ---------------------------------------------------------------------------

tests :: [TestCase]
tests =
    -- Original GPIO toggle demo (uses the pre-built example program)
    [ TestCase
        { tcName     = "test_gpio_demo"
        , tcProgBin  = "example/Example/program.bin"
        , tcTbVhd    = "tests/ghdl/avr_soc_tb.vhd"
        , tcStopNs   = 2000
        , tcExpected = [("gpio_port", "0x1"), ("gpio_ddr", "0x255")]
          -- program.S is the SREG alias-read demo: PORT_A = SREG with carry set
          -- = 1 (proves the register-alias read path); DDR = 0xFF = 255 decimal.
        }

    -- Signed Ramp peripheral end-to-end (PLAN_TYPED_HDL #3d): CPU writes STEP/
    -- SETPOINT=-6 over the bus, the ramp's signed FSM converges to -6, the CPU
    -- reads CURRENT back and drives it onto PORT_A = 0xFA (= 250 = signed -6).
    , TestCase
        { tcName     = "test_ramp"
        , tcProgBin  = "tests/fixtures/ramp_demo.bin"
        , tcTbVhd    = "tests/ghdl/ramp_tb.vhd"
        , tcStopNs   = 2500
        , tcExpected = [("gpio_port", "0x250"), ("gpio_ddr", "0x255")]
          -- 0x250 = "0x" prefix + image(250); 250 = 0xFA = signed -6.
        }

    -- ALU instruction test: chain of ADD/SUB/AND/OR/EOR/INC/DEC/LSR/ASR/NEG/COM/SWAP
    -- Final GPIO_PORT = 0xBA (186 decimal)
    , TestCase
        { tcName     = "test_alu"
        , tcProgBin  = "tests/fixtures/alu_test.bin"
        , tcTbVhd    = "tests/ghdl/alu_tb.vhd"
        , tcStopNs   = 1000
        , tcExpected = [("gpio_port", "0x186"), ("gpio_ddr", "0x255")]
        }

    -- MUL/MULS test: 200*3=600 and (-3)*5=-15, proving the 16-bit product is
    -- genuinely split across R1:R0. MUL writes two registers (R0 low, R1 high);
    -- the synth sequences them across two cycles through the single rf write
    -- port. Folds all four product bytes into GPIO_PORT = 0xAA (170 decimal).
    , TestCase
        { tcName     = "test_mul"
        , tcProgBin  = "tests/fixtures/mul_test.bin"
        , tcTbVhd    = "tests/ghdl/mul_tb.vhd"
        , tcStopNs   = 1000
        , tcExpected = [("gpio_port", "0x170"), ("gpio_ddr", "0x255")]
        }

    -- Z-pointer test: proves Z is a VIEW over R31:R30 (the GPR file). Sets Z via
    -- ldi r30/r31, ST/LD through Z. Previously impossible (avrZ was separate
    -- storage). GPIO_PORT = 0xC3 (195 decimal).
    , TestCase
        { tcName     = "test_zptr"
        , tcProgBin  = "tests/fixtures/zptr_test.bin"
        , tcTbVhd    = "tests/ghdl/zptr_tb.vhd"
        , tcStopNs   = 1000
        , tcExpected = [("gpio_port", "0x195"), ("gpio_ddr", "0x255")]
        }

    -- GPR-in-data-space test: aliasFile maps the register file at 0x00, so a read
    -- of data address 0x05 returns R5. GPIO_PORT = 0x99 (153 decimal).
    , TestCase
        { tcName     = "test_gpralias"
        , tcProgBin  = "tests/fixtures/gpralias_test.bin"
        , tcTbVhd    = "tests/ghdl/gpralias_tb.vhd"
        , tcStopNs   = 1000
        , tcExpected = [("gpio_port", "0x153"), ("gpio_ddr", "0x255")]
        }

    -- Multi-byte alias test: SP is 16-bit, aliased at 0x5D, so SPL=0x5D and
    -- SPH=0x5E (endian-correct). Write SP=0x1234 via SPL/SPH, read SPH = 0x12.
    , TestCase
        { tcName     = "test_sph"
        , tcProgBin  = "tests/fixtures/sph_test.bin"
        , tcTbVhd    = "tests/ghdl/sph_tb.vhd"
        , tcStopNs   = 1000
        , tcExpected = [("gpio_port", "0x18"), ("gpio_ddr", "0x255")]
        }

    -- GPR file WRITE via data space: sts 0x07 writes R7 (the file is just a block
    -- of registers; a store in the alias window is another writer). GPIO = 0x7E.
    , TestCase
        { tcName     = "test_gprwrite"
        , tcProgBin  = "tests/fixtures/gprwrite_test.bin"
        , tcTbVhd    = "tests/ghdl/gprwrite_tb.vhd"
        , tcStopNs   = 1000
        , tcExpected = [("gpio_port", "0x126"), ("gpio_ddr", "0x255")]
        }

    -- Branch / subroutine test: RJMP + RCALL/RET + BRNE countdown loop
    -- Accumulates 5+4+3+2+1 = 15 = 0x0F → GPIO_PORT = 15 decimal
    , TestCase
        { tcName     = "test_branch"
        , tcProgBin  = "tests/fixtures/branch_test.bin"
        , tcTbVhd    = "tests/ghdl/branch_tb.vhd"
        , tcStopNs   = 3000
        , tcExpected = [("gpio_port", "0x15"), ("gpio_ddr", "0x255")]
        }

    -- Memory test: STS/LDS + Z-pointer ST/LD + PUSH/POP
    -- All paths produce GPIO_PORT = 0xFF (255 decimal)
    , TestCase
        { tcName     = "test_mem"
        , tcProgBin  = "tests/fixtures/mem_test.bin"
        , tcTbVhd    = "tests/ghdl/mem_tb.vhd"
        , tcStopNs   = 2000
        , tcExpected = [("gpio_port", "0x255"), ("gpio_ddr", "0x255")]
        }

    -- Immediate instruction test: SUBI/ANDI/ORI + CPI+BRCC skip
    -- Final GPIO_PORT = 0x4C (76 decimal)
    , TestCase
        { tcName     = "test_imm"
        , tcProgBin  = "tests/fixtures/imm_test.bin"
        , tcTbVhd    = "tests/ghdl/imm_tb.vhd"
        , tcStopNs   = 500
        , tcExpected = [("gpio_port", "0x76"), ("gpio_ddr", "0x255")]
        }

    -- Timer peripheral test: write/read Timer OCR register
    -- Final GPIO_PORT = 0xA5 (165 decimal)
    , TestCase
        { tcName     = "test_timer"
        , tcProgBin  = "tests/fixtures/timer_test.bin"
        , tcTbVhd    = "tests/ghdl/timer_tb.vhd"
        , tcStopNs   = 500
        , tcExpected = [("gpio_port", "0x165"), ("gpio_ddr", "0x255")]
        }

    -- UART peripheral test: write/read UART UBRR register
    -- Final GPIO_PORT = 0x68 (104 decimal)
    , TestCase
        { tcName     = "test_uart"
        , tcProgBin  = "tests/fixtures/uart_test.bin"
        , tcTbVhd    = "tests/ghdl/uart_tb.vhd"
        , tcStopNs   = 500
        , tcExpected = [("gpio_port", "0x104"), ("gpio_ddr", "0x255")]
        }

    -- Instruction-coverage: control flow — JMP, CALL/RET, RCALL, ICALL/IJMP,
    -- RETI, BRBS.  Accumulator reaches 0x44 only if every call returned right.
    , TestCase
        { tcName     = "cov_flow"
        , tcProgBin  = "tests/fixtures/cov_flow.bin"
        , tcTbVhd    = "tests/ghdl/cov_flow_tb.vhd"
        , tcStopNs   = 4000
        , tcExpected = [("gpio_port", "0x68"), ("gpio_ddr", "0x255")]
        }

    -- Instruction-coverage: carry arithmetic / compares / skips / 16-bit ops —
    -- ADC, SBC, SBCI, ROR, CP, CPC, CPSE, SBRC, SBRS, ADIW, SBIW.
    -- Check-and-poison chain ends at 0xA5 only if all results are correct.
    , TestCase
        { tcName     = "cov_arith"
        , tcProgBin  = "tests/fixtures/cov_arith.bin"
        , tcTbVhd    = "tests/ghdl/cov_arith_tb.vhd"
        , tcStopNs   = 2500
        , tcExpected = [("gpio_port", "0x165"), ("gpio_ddr", "0x255")]
        }

    -- Instruction-coverage: I/O-space + bit ops — IN, OUT, SBI, CBI, SBIC, SBIS
    -- (via the bit-addressable test GPIO at 0x20), BSET, BCLR, BST, BLD.
    -- Ends at 0x5A only if every op (incl. SBI/CBI read-modify-write) is correct.
    , TestCase
        { tcName     = "cov_io"
        , tcProgBin  = "tests/fixtures/cov_io.bin"
        , tcTbVhd    = "tests/ghdl/cov_io_tb.vhd"
        , tcStopNs   = 2500
        , tcExpected = [("gpio_port", "0x90"), ("gpio_ddr", "0x255")]
        }

    -- Interrupt end-to-end (uses the avr-irq-soc-synth SoC): the timer overflow
    -- drives a createIrq latching controller into the CPU, which vectors to the
    -- ISR that writes 0x55 to GPIO PORT.  gpio_port = 0x55 proves the IRQ actually
    -- fired and the CPU vectored; gpio_ddr = 0xFF proves main() ran.
    , TestCase
        { tcName     = "test_irq"
        , tcProgBin  = "tests/fixtures/interrupt_test.bin"
        , tcTbVhd    = "tests/ghdl/irq_tb.vhd"
        , tcStopNs   = 12000
        , tcExpected = [("gpio_port", "0x85"), ("gpio_ddr", "0x255")]
        }

    -- Wishbone wait-state stall (uses the avr-wb-soc-synth SoC): the data bus is
    -- a Wishbone (stalling) bus, and a wait-state slave on a nested Wishbone
    -- sub-bus (bus-to-bus) holds a read for 5 cycles then returns 5.  The program
    -- LDSes it into a register and drives GPIO PORT.  gpio_port = 5 proves the CPU
    -- held through the bus stall; a CPU ignoring stall would read the early 0.
    , TestCase
        { tcName     = "test_wb_wait"
        , tcProgBin  = "tests/fixtures/wb_wait.bin"
        , tcTbVhd    = "tests/ghdl/wb_soc_tb.vhd"
        , tcStopNs   = 2000
        , tcExpected = [("gpio_port", "0x5"), ("gpio_ddr", "0x255")]
        }
    ]

-- | Which SoC synth executable a case uses.  All but the interrupt case use the
-- coverage SoC; @test_irq@ uses the interrupt-demo SoC (timer → createIrq → CPU).
synthFor :: String -> String
synthFor "test_irq"     = "avr-irq-soc-synth"
synthFor "test_wb_wait" = "avr-wb-soc-synth"
synthFor _              = "avr-soc-synth"

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

runTest :: TestCase -> IO (String, [String])
runTest tc = do
    (rc, out, err) <- readProcessWithExitCode "bash"
        [ "tests/ghdl/run_test.sh"
        , tcName tc
        , tcProgBin tc
        , tcTbVhd tc
        , show (tcStopNs tc)
        , synthFor (tcName tc)
        ] ""
    let combined = out ++ err
    let failures =
            [ "MISSING token \"" ++ token ++ "\""
            | (sig, want) <- tcExpected tc
            , let token = sig ++ " = " ++ want
            , not (token `isInfixOf` combined)
            ]
        allFails =
            if rc /= ExitSuccess
                then "non-zero exit code from ghdl/synth" : failures
                else failures
    if null allFails
        then return (tcName tc, [])
        else return (tcName tc, allFails ++ ["--- output ---", combined])

main :: IO ()
main = do
    results <- mapM runTest tests
    let failed = [(name, msgs) | (name, msgs) <- results, not (null msgs)]
    if null failed
        then do
            mapM_ (\(name, _) -> putStrLn $ "PASS  " ++ name) results
            putStrLn "ghdl-sim: all tests passed"
            exitSuccess
        else do
            mapM_ (\(name, _) -> putStrLn $ "PASS  " ++ name)
                  [(n, m) | (n, m) <- results, null m]
            mapM_ (\(name, msgs) -> do
                hPutStrLn stderr $ "FAIL  " ++ name
                mapM_ (hPutStrLn stderr . ("  " ++)) msgs)
                  failed
            exitFailure
