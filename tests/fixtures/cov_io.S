; cov_io.S — VHDL instruction-coverage: I/O-space and bit instructions.
;
; Uses the bit-addressable test GPIO mapped at data 0x20 (I/O 0x00–0x02); its
; PORT is data 0x22 = I/O 0x02 — the only register SBI/CBI/SBIC/SBIS can reach.
; Check-and-poison: reaching the end (GPIO_PORT = 0x5A) proves every op ran.
;
;   OUT/IN       write then read back gpiot PORT (0x3C)
;   SBI/CBI      set bits 0,1 then clear bit 2 of PORT: 0x3C -> 0x3F -> 0x3B
;                (read-modify-write of one I/O register in a single instruction)
;   SBIS/SBIC    skip on PORT bit0=1 (set) and bit2=0 (clear)
;   BSET/BCLR    set then clear the carry flag, checked via BRBC/BRBS
;   BST/BLD      copy bit3 of 0xFF through T into bit5 of r18 (-> 0x20)
;
; Final state: GPIO_PORT = 0x5A (90 decimal).

.arch avr6
.text
.org 0

    ldi   r16, 0xFF
    sts   0x0061, r16     ; main GPIO DDR = 0xFF

    ; OUT then IN on the test GPIO PORT (I/O 0x02)
    ldi   r16, 0x3C
    out   0x02, r16       ; gpiot PORT = 0x3C
    in    r17, 0x02       ; r17 = 0x3C
    cpi   r17, 0x3C
    brne  fail

    ; SBI / CBI — read-modify-write of the PORT register
    sbi   0x02, 0         ; 0x3C -> 0x3D
    sbi   0x02, 1         ; 0x3D -> 0x3F
    cbi   0x02, 2         ; 0x3F -> 0x3B
    in    r17, 0x02
    cpi   r17, 0x3B
    brne  fail

    ; SBIS: PORT bit0 (0x3B) is set -> skip poison
    sbis  0x02, 0
    rjmp  fail
    ; SBIC: PORT bit2 (0x3B) is clear -> skip poison
    sbic  0x02, 2
    rjmp  fail

    ; BSET / BCLR on the carry flag, verified by branch
    bset  0               ; C = 1
    brbc  0, fail         ; if C clear -> fail
    bclr  0               ; C = 0
    brbs  0, fail         ; if C set   -> fail

    ; BST / BLD through the T flag
    ldi   r16, 0xFF
    bst   r16, 3          ; T = bit3(0xFF) = 1
    ldi   r18, 0x00
    bld   r18, 5          ; r18 bit5 = T -> r18 = 0x20
    cpi   r18, 0x20
    brne  fail

    ldi   r16, 0x5A       ; all checks passed
    rjmp  done
fail:
    ldi   r16, 0xEE
done:
    sts   0x0062, r16     ; main GPIO PORT
halt:
    rjmp  halt
