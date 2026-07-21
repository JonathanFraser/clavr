-- ramp_tb.vhd — end-to-end testbench for the signed Ramp peripheral.
--
-- Runs the avr_soc with the ramp_demo program (tests/fixtures/ramp_demo.bin):
-- the CPU writes STEP=2 and SETPOINT=-6 to the ramp over the bus, the ramp's
-- signed FSM ramps CURRENT down to -6, and the CPU reads CURRENT back and drives
-- it onto PORT_A.  Observing gpio_port = 0xFA (= -6) proves the typed-HDL signed
-- datapath works end-to-end across the CPU bus (PLAN_TYPED_HDL #3d behavioural).
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ramp_tb is
end entity ramp_tb;

architecture sim of ramp_tb is
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

    clk <= not clk after 10 ns;   -- 50 MHz (fast for simulation)
    rst <= '0' after 25 ns;       -- release reset after ~1 cycle

    process
    begin
        wait for 2000 ns;   -- let the program run and the ramp converge
        assert gpio_ddr = to_unsigned(255, 8)
            report "FAIL: gpio_ddr = " & integer'image(to_integer(gpio_ddr))
                   & ", expected 255"
            severity error;
        -- -6 read back over the bus as an unsigned byte = 0xFA = 250.
        assert gpio_port = to_unsigned(16#FA#, 8)
            report "FAIL: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 0xFA (signed -6)"
            severity error;
        report "gpio_port = 0x" & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = 0x" & integer'image(to_integer(gpio_ddr))
            severity note;
        report "ramp CURRENT read back = signed "
               & integer'image(to_integer(signed(gpio_port)))
            severity note;
        report "RAMP END-TO-END CHECK PASSED" severity note;
        std.env.stop;
    end process;
end architecture sim;
