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
SYNTH="${5:-avr-soc-synth}"   # which SoC synth executable to run (default: coverage SoC)
EXTRA_VHD="${6:-}"            # extra hand-written VHDL (e.g. an external Wishbone slave)

if [ -z "$NAME" ] || [ -z "$PROG" ] || [ -z "$TB_VHD" ] || [ -z "$STOP_NS" ]; then
    echo "Usage: run_test.sh <name> <prog.bin> <testbench.vhd> <stop-time-ns> [synth-exe] [extra.vhd]" >&2
    exit 1
fi

TB_ENTITY="$(basename "$TB_VHD" .vhd)"
OUTDIR="build/${NAME}"
WORKDIR="${OUTDIR}/ghdl_work"

# 1. Synthesise VHDL with the given program binary.  Clear any stale entities
#    first so the analyse step sees exactly this design's sub-entities (their set
#    and names depend on the SoC — e.g. inlined vs. sub-entity peripherals).
rm -f "$OUTDIR"/*.vhd
cabal run "$SYNTH" -- "$PROG" "$OUTDIR"

mkdir -p "$WORKDIR"

# 2. Analyse every synthesised design file + the testbench into an isolated work
#    library.  ghdl resolves direct entity instantiations at analysis time, so a
#    sub-entity must be analysed before the top entity that instantiates it:
#    analyse all sub-entities first (they are leaves, order among them is free),
#    then avr_soc, then the testbench.  Globbing means the list need not track
#    entity names (they depend on the SoC — e.g. inlined vs. sub-entity gpio).
#    Any EXTRA_VHD (a hand-written entity instantiated by the design, e.g. an
#    external Wishbone slave) is a leaf too and analysed alongside the sub-entities.
SUBS=$(ls "$OUTDIR"/*.vhd | grep -v '/avr_soc\.vhd$')
ghdl -a --std=08 --workdir="$WORKDIR" $SUBS $EXTRA_VHD "$OUTDIR/avr_soc.vhd" "$TB_VHD"

# 3. Elaborate
ghdl -e --std=08 --workdir="$WORKDIR" "$TB_ENTITY"

# 4. Simulate
ghdl -r --std=08 --workdir="$WORKDIR" "$TB_ENTITY" \
    --vcd="${OUTDIR}/${TB_ENTITY}.vcd" \
    --stop-time="${STOP_NS}ns"

echo "VCD: ${OUTDIR}/${TB_ENTITY}.vcd"
