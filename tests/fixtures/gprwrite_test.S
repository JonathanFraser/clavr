; gprwrite_test.S — data-space WRITE to the register file: sts 0x07 writes R7.
; Then read R7 directly (mov r17, r7) and show it.
.arch avr6
.text
.org 0
    ldi   r16, 0xFF
    sts   0x0061, r16       ; DDR = 0xFF
    ldi   r16, 0x7E
    sts   0x0007, r16       ; data[0x07] = R7  (file alias write)
    mov   r17, r7           ; r17 = R7
    sts   0x0062, r17       ; GPIO = R7 = 0x7E
halt:
    rjmp  halt
