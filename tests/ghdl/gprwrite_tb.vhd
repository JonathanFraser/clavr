-- gprwrite_tb.vhd — testbench for gprwrite_test.S
--
-- Program runs MUL (200*3=600=0x0258) and MULS ((-3)*5=-15=0xFFF1), folds all
-- four product bytes into one observable, and writes it to GPIO_PORT.
--
-- This proves the product is a real 16-bit value split across R1:R0 — the old
-- bug truncated to 8 bits and wrote the same byte to both, which would NOT
-- produce 0xAA.
--
-- Expected at 1000 ns:
--   gpio_ddr  = 0xFF  (255 decimal)
--   gpio_port = 0xAA  (170 decimal)
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity gprwrite_tb is
end entity gprwrite_tb;

architecture sim of gprwrite_tb is
    signal clk       : std_logic := '0';
    signal rst       : std_logic := '1';
    signal gpio_port : unsigned(7 downto 0);
    signal gpio_ddr  : unsigned(7 downto 0);
begin
    dut : entity work.avr_soc port map (
        dom10mhz                      => clk,
        rst                           => rst,
        uart0_UartPhys_uartTxLine     => open,
        uart0_UartPhys_uartRxIrq      => open,
        uart0_UartPhys_uartTxIrq      => open,
        timer0_TimerPhys_timerOvfIrq  => open,
        timer0_TimerPhys_timerCmpIrq  => open,
        gpio0_GpioPhys_gpioPort       => gpio_port,
        gpio0_GpioPhys_gpioDdr        => gpio_ddr,
        gpio_port                     => open,
        gpio_ddr                      => open
    );

    clk <= not clk after 10 ns;
    rst <= '0' after 25 ns;

    process
    begin
        wait for 1000 ns;
        assert gpio_ddr = to_unsigned(255, 8)
            report "FAIL gprwrite_tb: gpio_ddr = " & integer'image(to_integer(gpio_ddr))
                   & ", expected 255"
            severity error;
        assert gpio_port = to_unsigned(126, 8)
            report "FAIL gprwrite_tb: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 126 (0x7E)"
            severity error;
        report "gpio_port = 0x" & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = 0x" & integer'image(to_integer(gpio_ddr));
        wait for 200 ns;
        std.env.stop;
    end process;
end architecture sim;
