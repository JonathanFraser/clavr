-- irq_tb.vhd — end-to-end interrupt test for the createIrq controller.
--
-- The interrupt-demo SoC (avr-irq-soc-synth) enables the timer (ticks every
-- cycle, overflows at 256), routes its overflow through a createIrq latching
-- controller into the CPU (vector 0x0020), and runs interrupt_test.S: main sets
-- DDRA=0xFF, SEI, spins; on the timer overflow the CPU vectors to the ISR, which
-- writes 0x55 to GPIO PORT and RETIs.
--
-- Expected once the timer has overflowed (well before 12000 ns):
--   gpio_ddr  = 0xFF (255)  — main() ran (DDR write)
--   gpio_port = 0x55 (85)   — the ISR ran → the IRQ genuinely vectored the CPU
-- If interrupts were broken, gpio_port would remain 0x00.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity irq_tb is
end entity irq_tb;

architecture sim of irq_tb is
    signal clk       : std_logic := '0';
    signal rst       : std_logic := '1';
    signal gpio_port : unsigned(7 downto 0);
    signal gpio_ddr  : unsigned(7 downto 0);
begin
    dut : entity work.avr_soc port map (
        dom10mhz                      => clk,
        rst                           => rst,
        uart_rx                       => '0',
        gpio_in                     => "00000000",
        gpio_port                     => gpio_port,
        gpio_ddr                      => gpio_ddr,
        gpiot_port                    => open
    );

    clk <= not clk after 10 ns;
    rst <= '0' after 25 ns;

    process
    begin
        -- The first timer overflow is at ~256 cycles (~5120 ns); give the handler
        -- and ISR ample margin.
        wait for 12000 ns;
        assert gpio_ddr = to_unsigned(255, 8)
            report "FAIL irq_tb: gpio_ddr = " & integer'image(to_integer(gpio_ddr))
                   & ", expected 255 (main did not run)"
            severity error;
        assert gpio_port = to_unsigned(85, 8)
            report "FAIL irq_tb: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 85 (0x55): the ISR did not run, IRQ did not vector"
            severity error;
        report "gpio_port = 0x" & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = 0x" & integer'image(to_integer(gpio_ddr));
        wait for 200 ns;
        std.env.stop;
    end process;
end architecture sim;
