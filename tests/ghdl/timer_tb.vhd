-- timer_tb.vhd — testbench for timer_test.S
--
-- Program writes TCCR=0x01 (CTC mode) and OCR=0xA5 to the Timer peripheral,
-- reads OCR back, and writes it to GPIO_PORT.  Exercises the Timer register
-- read/write bus paths without relying on interrupt delivery.
--
-- Expected at 500 ns:
--   gpio_ddr  = 0xFF  (255 decimal)
--   gpio_port = 0xA5  (165 decimal)
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity timer_tb is
end entity timer_tb;

architecture sim of timer_tb is
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
        gpio_ddr                      => gpio_ddr
    );

    clk <= not clk after 10 ns;
    rst <= '0' after 25 ns;

    process
    begin
        wait for 500 ns;
        assert gpio_ddr = to_unsigned(255, 8)
            report "FAIL timer_tb: gpio_ddr = " & integer'image(to_integer(gpio_ddr))
                   & ", expected 255"
            severity error;
        assert gpio_port = to_unsigned(165, 8)
            report "FAIL timer_tb: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 165 (0xA5)"
            severity error;
        report "gpio_port = 0x" & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = 0x" & integer'image(to_integer(gpio_ddr));
        wait for 200 ns;
        std.env.stop;
    end process;
end architecture sim;
