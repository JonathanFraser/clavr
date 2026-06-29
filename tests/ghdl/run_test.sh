#!/usr/bin/env bash
# run_test.sh — synthesise, compile, and simulate one AVR SoC test case.
#
# Usage: run_test.sh <name> <prog.bin> <testbench.vhd> <stop-time-ns>
#
#   name          short identifier used as the output directory name
#   prog.bin      flat AVR binary to bake into the ROM
#   testbench.vhd VHDL testbench file (entity name = basename without .vhd)
#   stop-time-ns  simulation stop time in nanoseconds
#
# The synthesised VHDL and GHDL work library are placed under build/<name>/.
# Script must be run from the clavr project root.
set -e

NAME="$1"
PROG="$2"
TB_VHD="$3"
STOP_NS="$4"

if [ -z "$NAME" ] || [ -z "$PROG" ] || [ -z "$TB_VHD" ] || [ -z "$STOP_NS" ]; then
    echo "Usage: run_test.sh <name> <prog.bin> <testbench.vhd> <stop-time-ns>" >&2
    exit 1
fi

TB_ENTITY="$(basename "$TB_VHD" .vhd)"
OUTDIR="build/${NAME}"
WORKDIR="${OUTDIR}/ghdl_work"

# 1. Synthesise VHDL with the given program binary
cabal run avr-soc-synth -- "$PROG" "$OUTDIR"

mkdir -p "$WORKDIR"

# 2. Analyse all design files + testbench into an isolated work library
ghdl -a --std=08 --workdir="$WORKDIR" \
    "$OUTDIR/cpu.vhd" \
    "$OUTDIR/uart0.vhd" \
    "$OUTDIR/timer0.vhd" \
    "$OUTDIR/gpio0.vhd" \
    "$OUTDIR/gpiot.vhd" \
    "$OUTDIR/ramp0.vhd" \
    "$OUTDIR/ram0.vhd" \
    "$OUTDIR/databus.vhd" \
    "$OUTDIR/avr_soc.vhd" \
    "$TB_VHD"

# 3. Elaborate
ghdl -e --std=08 --workdir="$WORKDIR" "$TB_ENTITY"

# 4. Simulate
ghdl -r --std=08 --workdir="$WORKDIR" "$TB_ENTITY" \
    --vcd="${OUTDIR}/${TB_ENTITY}.vcd" \
    --stop-time="${STOP_NS}ns"

echo "VCD: ${OUTDIR}/${TB_ENTITY}.vcd"
