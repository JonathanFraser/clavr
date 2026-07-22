; wb_wait.S — Wishbone wait-state stall proof.
;
; The data bus is a Wishbone (stalling) bus.  A read of the wait-state slave at
; data address 0x0080 asserts the bus @stall@ for 5 cycles and only then returns
; 5.  If the CPU honours stall it holds the LDS until the value settles and reads
; 5; a CPU that ignored stall would sample the early counter value (0).
;
; Final state: GPIO_DDR = 0xFF, GPIO_PORT = 5  (proves the CPU held on stall)

.arch avr6
.text
.org 0

    ; GPIO: all pins output
    ldi   r16, 0xFF
    sts   0x0061, r16       ; DDR = 0xFF

    ; Read the wait-state slave — this LDS stalls for 5 cycles, then reads 5.
    lds   r17, 0x0080       ; r17 = 5 iff the CPU held through the stall
    sts   0x0062, r17       ; GPIO_PORT = r17

halt:
    rjmp  halt
