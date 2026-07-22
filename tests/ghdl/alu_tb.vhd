-- alu_tb.vhd — testbench for alu_test.S
--
-- Program computes a chain of ALU ops ending with SWAP 0xAB → 0xBA,
-- then writes the result to GPIO_PORT.
--
-- Expected at 1000 ns:
--   gpio_ddr  = 0xFF  (255 decimal)
--   gpio_port = 0xBA  (186 decimal)
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity alu_tb is
end entity alu_tb;

architecture sim of alu_tb is
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
        wait for 1000 ns;
        assert gpio_ddr = to_unsigned(255, 8)
            report "FAIL alu_tb: gpio_ddr = " & integer'image(to_integer(gpio_ddr))
                   & ", expected 255"
            severity error;
        assert gpio_port = to_unsigned(186, 8)
            report "FAIL alu_tb: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 186 (0xBA)"
            severity error;
        report "gpio_port = 0x" & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = 0x" & integer'image(to_integer(gpio_ddr));
        wait for 200 ns;
        std.env.stop;
    end process;
end architecture sim;
