-- uart_tb.vhd — testbench for uart_test.S
--
-- Program writes UBRR=0x68 (104 decimal) to the UART baud-rate register,
-- reads it back, and writes it to GPIO_PORT.  Exercises the UART register
-- read/write bus paths.
--
-- Expected at 500 ns:
--   gpio_ddr  = 0xFF  (255 decimal)
--   gpio_port = 0x68  (104 decimal)
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity uart_tb is
end entity uart_tb;

architecture sim of uart_tb is
    signal clk       : std_logic := '0';
    signal rst       : std_logic := '1';
    signal gpio_port : unsigned(7 downto 0);
    signal gpio_ddr  : unsigned(7 downto 0);
begin
    dut : entity work.avr_soc port map (
        dom10mhz                      => clk,
        rst                           => rst,
        uart_rx                       => '0',
        gpio_a_in                     => "00000000",
        uart0_UartPhys_uartTxLine     => open,
        uart0_UartPhys_uartRxIrq      => open,
        uart0_UartPhys_uartTxIrq      => open,
        timer0_TimerPhys_timerOvfIrq  => open,
        timer0_TimerPhys_timerCmpIrq  => open,
        gpio_port                     => gpio_port,
        gpio_ddr                      => gpio_ddr
    );

    clk <= not clk after 10 ns;
    rst <= '0' after 25 ns;

    process
    begin
        wait for 500 ns;
        assert gpio_ddr = to_unsigned(255, 8)
            report "FAIL uart_tb: gpio_ddr = " & integer'image(to_integer(gpio_ddr))
                   & ", expected 255"
            severity error;
        assert gpio_port = to_unsigned(104, 8)
            report "FAIL uart_tb: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 104 (0x68)"
            severity error;
        report "gpio_port = 0x" & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = 0x" & integer'image(to_integer(gpio_ddr));
        wait for 200 ns;
        std.env.stop;
    end process;
end architecture sim;
