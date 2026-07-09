; sph_test.S — proves a 16-bit register aliased at a base spans two data
; addresses, endian-correct: SPL at 0x5D (low), SPH at 0x5E (high).
; Write SP = 0x1234 via SPL/SPH, then read SPH back: must be 0x12, not 0x34.
.arch avr6
.text
.org 0
    ldi   r16, 0xFF
    sts   0x0061, r16       ; DDR = 0xFF
    ldi   r16, 0x34
    sts   0x005D, r16       ; SPL = 0x34
    ldi   r16, 0x12
    sts   0x005E, r16       ; SPH = 0x12   -> SP = 0x1234
    lds   r17, 0x005E       ; r17 = SPH = 0x12  (high byte, little-endian)
    sts   0x0062, r17       ; GPIO = 0x12
halt:
    rjmp  halt
