-- wb_extern_tb.vhd — testbench for wb_extern.S with a hand-written Wishbone slave.
--
-- The program reads the externally-authored Wishbone slave at data 0x0090 (4 wait
-- states, then ack + 0x3B) and drives the result onto GPIO_PORT.  Reading 59
-- proves the AVR completed a classic ACK-terminated transfer against a slave it
-- did not generate.
--
-- Expected:
--   gpio_ddr  = 0xFF  (255 decimal)
--   gpio_port = 0x3B  (59 decimal)
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity wb_extern_tb is
end entity wb_extern_tb;

architecture sim of wb_extern_tb is
    signal clk       : std_logic := '0';
    signal rst       : std_logic := '1';
    signal gpio_port : unsigned(7 downto 0);
    signal gpio_ddr  : unsigned(7 downto 0);
begin
    dut : entity work.avr_soc port map (
        dom10mhz  => clk,
        rst       => rst,
        gpio_in   => "00000000",
        gpio_port => gpio_port,
        gpio_ddr  => gpio_ddr
    );

    clk <= not clk after 10 ns;
    rst <= '0' after 25 ns;

    process
    begin
        wait for 1000 ns;
        assert gpio_ddr = to_unsigned(255, 8)
            report "FAIL wb_extern_tb: gpio_ddr = " & integer'image(to_integer(gpio_ddr))
                   & ", expected 255"
            severity error;
        assert gpio_port = to_unsigned(59, 8)
            report "FAIL wb_extern_tb: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 59 (0x3B; CPU completed the ACK handshake)"
            severity error;
        report "gpio_port = 0x" & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = 0x" & integer'image(to_integer(gpio_ddr));
        wait for 200 ns;
        std.env.stop;
    end process;
end architecture sim;
