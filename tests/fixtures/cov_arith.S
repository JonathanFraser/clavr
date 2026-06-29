; cov_arith.S — VHDL instruction-coverage: carry-aware arithmetic, compares,
; skips, and 16-bit word ops.
;
; Each step computes a result and immediately checks it; any mismatch diverts to
; the 'fail' sink (0xEE).  Reaching the end proves every instruction below ran
; correctly in synthesised hardware, so GPIO_PORT = 0xA5 on success.
;
;   ADC          0x10 + 0x20 + C(1)        = 0x31
;   SBC          0x31 - 0x20 - C(0)        = 0x11
;   SBCI         0x11 - 0x01 - C(0)        = 0x10
;   ROR          0x10 ror C(0)             = 0x08
;   CP   + BRBS  0x08 < 0x20 -> C=1        (branch taken)
;   CPC  + BRBS  0x08 - 0x08 - 0 -> Z=1    (branch taken)
;   CPSE         r16==r22 -> skip poison
;   SBRC         bit0(0x08)=0 -> skip poison
;   SBRS         bit3(0x08)=1 -> skip poison
;   ADIW         0x00FF + 1 = 0x0100       (carry into high byte)
;   SBIW         0x0100 - 1 = 0x00FF
;
; Final state: GPIO_PORT = 0xA5 (165 decimal).

.arch avr6
.text
.org 0

    ldi   r16, 0xFF
    sts   0x0061, r16     ; main GPIO DDR = 0xFF

    ; ADC — produce a carry, then add it in
    ldi   r16, 0xFF
    ldi   r17, 0x01
    add   r16, r17        ; r16 = 0x00, C = 1
    ldi   r16, 0x10
    ldi   r17, 0x20
    adc   r16, r17        ; r16 = 0x10 + 0x20 + 1 = 0x31
    cpi   r16, 0x31
    brne  fail

    ; SBC (C = 0 here)
    ldi   r17, 0x20
    sbc   r16, r17        ; r16 = 0x11
    cpi   r16, 0x11
    brne  fail

    ; SBCI
    sbci  r16, 0x01       ; r16 = 0x10
    cpi   r16, 0x10
    brne  fail

    ; ROR (C = 0 in)
    ror   r16             ; r16 = 0x08
    cpi   r16, 0x08
    brne  fail

    ; CP + BRBS(C): 0x08 < 0x20 -> carry set
    ldi   r17, 0x20
    cp    r16, r17
    brbs  0, cp_ok
    rjmp  fail
cp_ok:

    ; CPC + BRBS(Z): clear carry, then 0x08 - 0x08 - 0 = 0
    ldi   r21, 0x02
    lsr   r21             ; C = 0
    ldi   r19, 0x08
    cpc   r16, r19        ; Z = 1
    brbs  1, cpc_ok
    rjmp  fail
cpc_ok:

    ; CPSE / SBRC / SBRS — each must skip its poison
    ldi   r22, 0x08
    cpse  r16, r22        ; r16 == r22 -> skip
    rjmp  fail
    sbrc  r16, 0          ; bit0 clear -> skip
    rjmp  fail
    sbrs  r16, 3          ; bit3 set -> skip
    rjmp  fail

    ; ADIW / SBIW — 16-bit add/sub across the byte boundary on r25:r24
    ldi   r24, 0xFF
    ldi   r25, 0x00
    adiw  r24, 0x01       ; r25:r24 = 0x0100
    cpi   r24, 0x00
    brne  fail
    cpi   r25, 0x01
    brne  fail
    sbiw  r24, 0x01       ; r25:r24 = 0x00FF
    cpi   r24, 0xFF
    brne  fail
    cpi   r25, 0x00
    brne  fail

    ldi   r16, 0xA5       ; all checks passed
    rjmp  out
fail:
    ldi   r16, 0xEE
out:
    sts   0x0062, r16     ; main GPIO PORT
halt:
    rjmp  halt
