; gpralias_test.S — proves the GPR file is mapped into data space at 0x00.
; Set R5 (via mov, since ldi only hits r16-r31), then read data address 0x05 —
; with `aliasFile gpr 0x00` that read must return R5.
.arch avr6
.text
.org 0
    ldi   r16, 0xFF
    sts   0x0061, r16       ; DDR = 0xFF
    ldi   r16, 0x99
    mov   r5,  r16          ; R5 = 0x99
    lds   r17, 0x0005       ; r17 = data[0x05] = R5 = 0x99 (file alias)
    sts   0x0062, r17       ; GPIO = 0x99
halt:
    rjmp  halt
