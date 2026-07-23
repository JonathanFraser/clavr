; wb_extern.S — read a HAND-WRITTEN Wishbone slave over the bus.
;
; The slave at data 0x0090 inserts 4 Wishbone wait states, then acks and returns
; 0x3B (=59); before ack its data reads 0.  So GPIO_PORT = 59 only if the CPU
; honoured the classic ACK handshake against the externally-authored slave.
;
; Final state: GPIO_DDR = 0xFF, GPIO_PORT = 59

.arch avr6
.text
.org 0

    ; GPIO: all pins output
    ldi   r16, 0xFF
    sts   0x0061, r16       ; DDR = 0xFF

    ; Read the hand-written Wishbone slave (ACK-terminated, 4 wait states).
    lds   r17, 0x0090       ; r17 = 0x3B iff the CPU waited for wb_ack_o
    sts   0x0062, r17       ; GPIO_PORT = r17

halt:
    rjmp  halt
