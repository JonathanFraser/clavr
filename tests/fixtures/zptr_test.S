; zptr_test.S — proves the Z pointer is a VIEW over R31:R30 (the GPR file),
; not separate storage.  This is the case mem_test.S explicitly omitted because
; the old model had avrZ as an independent register that `ldi r30/r31` never
; touched ("ld r16, Z would read from address 0").
;
;   Z := 0x0205 by loading R30 (low) and R31 (high)
;   ST Z, r16   writes mem[0x0205] = 0xC3   — address comes from R31:R30
;   LD r17, Z   reads it back
;
; Final state: GPIO_PORT = 0xC3 (195), GPIO_DDR = 0xFF (255).

.arch avr6
.text
.org 0

    ldi   r16, 0xFF
    sts   0x0061, r16       ; DDR = 0xFF

    ; Set the Z pointer via its constituent GPR registers.
    ldi   r31, 0x02         ; Z high byte (R31)
    ldi   r30, 0x05         ; Z low  byte (R30)  -> Z = 0x0205

    ldi   r16, 0xC3
    st    Z, r16            ; mem[Z=0x0205] = 0xC3
    ldi   r17, 0x00         ; clobber
    ld    r17, Z            ; r17 = mem[0x0205] = 0xC3
    sts   0x0062, r17       ; GPIO_PORT = 0xC3
halt:
    rjmp  halt
