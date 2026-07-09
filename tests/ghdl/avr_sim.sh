#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/../.."

# 1. Synthesise VHDL
cabal run avr-soc-synth

VHDL=build/avr_soc
TB=tests/ghdl

# 2. Analyse all design files + testbench
# (isacle_ram_*.vhd / isacle_rom_*.vhd are template files not instantiated by
#  any parent — all memory is inlined; do not pass them to ghdl)
ghdl -a --std=08 \
    "$VHDL/cpu.vhd" \
    "$VHDL/uart0.vhd" \
    "$VHDL/timer0.vhd" \
    "$VHDL/gpio0.vhd" \
    "$VHDL/gpiot.vhd" \
    "$VHDL/ramp0.vhd" \
    "$VHDL/ram0.vhd" \
    "$VHDL/databus.vhd" \
    "$VHDL/avr_soc.vhd" \
    "$TB/avr_soc_tb.vhd"

# 3. Elaborate
ghdl -e --std=08 avr_soc_tb

# 4. Simulate, dump VCD for waveform inspection
ghdl -r --std=08 avr_soc_tb \
    --vcd="$VHDL/avr_soc.vcd" \
    --stop-time=2000ns

echo "VCD written to $VHDL/avr_soc.vcd"
echo "View with: gtkwave $VHDL/avr_soc.vcd"
