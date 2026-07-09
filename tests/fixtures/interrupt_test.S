; interrupt_test.S — end-to-end validation of the createIrq interrupt controller.
;
; main() sets DDRA = 0xFF, enables interrupts (SEI), and spins.  The timer counts
; every cycle and overflows at 256; its overflow, fed through createIrq (a
; latching priority controller), asserts the CPU's irq_pending with vector 0x0020.
; The CPU pushes the return PC and vectors to the ISR at word address 0x20, which
; writes 0x55 to GPIO PORT (the observable) and RETIs.  createIrq's latch is
; cleared by the CPU's irq_ack, so the source (a 1-cycle overflow pulse) is held
; across the multi-cycle handler — the whole point of the latching controller.
;
; Expected: gpio_ddr = 0xFF (main ran), gpio_port = 0x55 (the ISR ran → the IRQ
; vectored).  If the interrupt never fired, gpio_port would still be 0x00.
.arch avr6
.text
.org 0
reset:
    ldi   r16, 0xFF
    sts   0x0061, r16       ; DDRA = 0xFF  (GPIO port drives out)
    sei                     ; I = 1  (BSET 7) — enable interrupts
spin:
    rjmp  spin              ; wait to be interrupted

.org 0x40                   ; byte 0x40 = word 0x20 = the createIrq vector
isr:
    ldi   r17, 0x55
    sts   0x0062, r17       ; GPIO PORT = 0x55  (proves the ISR body executed)
    reti                    ; pop PC, restore I, return to spin
