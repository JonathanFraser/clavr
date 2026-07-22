-- wb_soc_tb.vhd — testbench for wb_wait.S on the Wishbone (stalling) SoC.
--
-- The program reads the wait-state slave at data 0x0080 (which stalls the bus
-- for 5 cycles, then returns 5) and drives the result onto GPIO_PORT.  The CPU
-- holding through the stall is what yields 5; a CPU that ignored stall would
-- read the early counter value (0).
--
-- Expected:
--   gpio_ddr  = 0xFF  (255 decimal)
--   gpio_port = 5
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity wb_soc_tb is
end entity wb_soc_tb;

architecture sim of wb_soc_tb is
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
            report "FAIL wb_soc_tb: gpio_ddr = " & integer'image(to_integer(gpio_ddr))
                   & ", expected 255"
            severity error;
        assert gpio_port = to_unsigned(5, 8)
            report "FAIL wb_soc_tb: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 5 (CPU held through the Wishbone stall)"
            severity error;
        report "gpio_port = 0x" & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = 0x" & integer'image(to_integer(gpio_ddr));
        wait for 200 ns;
        std.env.stop;
    end process;
end architecture sim;
