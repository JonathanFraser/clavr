; cov_flow.S — VHDL instruction-coverage: control-flow / subroutine ops.
;
; Exercises the instructions no other GHDL fixture reaches:
;   JMP   (32-bit absolute jump, skips poison)
;   CALL  / RET    (subroutine that adds 0x11 to the accumulator)
;   RCALL / RET    (relative call to the same subroutine)
;   ICALL / IJMP   (indirect call/jump through Z = r31:r30)
;   RETI           (interrupt-return; here just a RET that also sets I)
;   BRBS           (branch if SREG bit set — Z bit)
;
; The accumulator r16 only reaches 0x44 if CALL, RCALL, ICALL and the RETI
; subroutine each returned correctly (4 × 0x11).  Every indirect/branch path
; has a poison (0xEE) on the not-taken side, so a wrong jump corrupts the byte.
;
; Final state: GPIO_PORT = 0x44 (68 decimal).

.arch avr6
.text
.org 0

    jmp   start            ; JMP — 32-bit absolute, must skip the poison below
    ldi   r16, 0xEE        ; poison: reached only if JMP fails
start:
    ldi   r16, 0xFF
    sts   0x0061, r16      ; main GPIO DDR = 0xFF
    ldi   r16, 0x00        ; accumulator = 0
    ldi   r17, 0x11        ; per-call increment

    call  add_r17          ; CALL + RET   -> acc = 0x11
    rcall add_r17          ; RCALL + RET  -> acc = 0x22

    ; IJMP through Z to ij_target (skip poison)
    ldi   r30, lo8(pm(ij_target))
    ldi   r31, hi8(pm(ij_target))
    ijmp
    ldi   r16, 0xEE        ; poison: reached only if IJMP fails
ij_target:

    ; ICALL through Z to add_r17 -> acc = 0x33
    ldi   r30, lo8(pm(add_r17))
    ldi   r31, hi8(pm(add_r17))
    icall

    call  add_r17_reti     ; CALL + RETI  -> acc = 0x44

    ; BRBS: make Z=1 then branch on it (bit 1 of SREG)
    ldi   r18, 0x05
    subi  r18, 0x05        ; r18 = 0, Z = 1
    brbs  1, brbs_ok       ; branch taken (Z set)
    ldi   r16, 0xEE        ; poison: reached only if BRBS fails
brbs_ok:

    sts   0x0062, r16      ; main GPIO PORT = acc = 0x44
halt:
    rjmp  halt

; --- subroutines ---------------------------------------------------------
add_r17:
    add   r16, r17
    ret

add_r17_reti:
    add   r16, r17
    reti
