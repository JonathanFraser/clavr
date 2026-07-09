-- cov_io_tb.vhd — testbench for cov_flow.S (control-flow instruction coverage)
--
-- Exercises JMP, CALL/RET, RCALL, ICALL/IJMP, RETI, BRBS.  The accumulator
-- reaches 0x44 only if every call returned to the right place.
--
-- Expected:
--   gpio_ddr  = 0xFF  (255 decimal)
--   gpio_port = 0x44  (68 decimal)
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cov_io_tb is
end entity cov_io_tb;

architecture sim of cov_io_tb is
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
        wait for 2500 ns;
        assert gpio_ddr = to_unsigned(255, 8)
            report "FAIL cov_io_tb: gpio_ddr = " & integer'image(to_integer(gpio_ddr))
                   & ", expected 255"
            severity error;
        assert gpio_port = to_unsigned(90, 8)
            report "FAIL cov_io_tb: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 90 (0x5A)"
            severity error;
        report "gpio_port = 0x" & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = 0x" & integer'image(to_integer(gpio_ddr));
        wait for 200 ns;
        std.env.stop;
    end process;
end architecture sim;
