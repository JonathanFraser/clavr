library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity avr_soc_tb is
end entity avr_soc_tb;

architecture sim of avr_soc_tb is
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
        gpio0_GpioPhys_gpioPort       => gpio_port,
        gpio0_GpioPhys_gpioDdr        => gpio_ddr,
        gpio_port                     => open,
        gpio_ddr                      => open
    );

    clk <= not clk after 10 ns;   -- 50 MHz (fast for simulation)
    rst <= '0' after 25 ns;       -- release reset after ~1 cycle

    process
    begin
        wait for 500 ns;   -- 25 clock cycles after reset release
        assert gpio_ddr = to_unsigned(255, 8)
            report "FAIL: gpio_ddr = " & integer'image(to_integer(gpio_ddr))
                   & ", expected 255"
            severity error;
        -- program.S is the SREG alias-read demo: it sets SREG.C (=0x01) and
        -- drives the alias-read SREG value onto PORT_A.  Expected 0x01 proves
        -- the register-alias read path (raw SRAM would read 0x00).
        assert gpio_port = to_unsigned(16#01#, 8)
            report "FAIL: gpio_port = " & integer'image(to_integer(gpio_port))
                   & ", expected 0x01 (alias-read SREG.C)"
            severity error;
        report "gpio_port = 0x" & integer'image(to_integer(gpio_port))
               & "  gpio_ddr = 0x" & integer'image(to_integer(gpio_ddr));
        wait for 500 ns;   -- run longer for waveform visibility
        std.env.stop;
    end process;
end architecture sim;
