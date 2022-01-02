-------------------------------------------------
--
-- A minimum viable UART
-- This file is part of the MX65 6502 Project
-- Copyright (c) 2022 Steve Teal
--
-------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity uart is
  port (
	rx			: in std_logic;
	tx			: out std_logic;
	clock		: in std_logic;
	reset		: in std_logic;
	rx_ready	: out std_logic;
	rx_read		: in std_logic;
	tx_start	: in std_logic;
	tx_ready	: out std_logic;
	tx_data		: in std_logic_vector(7 downto 0);
	rx_data		: out std_logic_vector(7 downto 0)
  );
end entity;

architecture rtl of uart is

	constant baud_top : std_logic_vector(6 downto 0) := "1101000";
	signal rx_active : std_logic;
	signal rx_baud_count : std_logic_vector(6 downto 0);
	signal rx_bit_count : std_logic_vector(3 downto 0);
	signal rx_sample : std_logic;
	signal rx_filter : std_logic_vector(2 downto 0);
	signal rx_input : std_logic;
	signal rx_stop : std_logic;
	signal rx_overrun : std_logic;
	signal rx_shift_reg : std_logic_vector(8 downto 0);
	signal tx_baud_count : std_logic_vector(6 downto 0);
	signal tx_bit_count : std_logic_vector(3 downto 0);
	signal tx_active : std_logic;
	signal tx_shift_reg : std_logic_vector(8 downto 0);

begin

	tx <= tx_shift_reg(0);
	tx_ready <= not tx_active;

	process(reset,clock)
	begin
		if reset='0' then
			rx_filter <= "000";
		elsif rising_edge(clock) then
			rx_filter <= rx_filter(1 downto 0) & rx;
		end if;
	end process;

	rx_input <= (rx_filter(0) and rx_filter(1)) or (rx_filter(1) and rx_filter(2)) or (rx_filter(0) and rx_filter(2));
	
	process(reset,clock)
	begin
		if reset='0' then
			rx_baud_count <= (others=>'0');
		elsif rising_edge(clock) then
			if rx_baud_count = baud_top or rx_active='0' then
				rx_baud_count <= (others=>'0');
			else
				rx_baud_count <= rx_baud_count + 1;
			end if;
		end if;
	end process;
	
	rx_stop <= '1' when rx_bit_count = "1001" and rx_sample = '1' else '0';
	rx_overrun <= '1' when rx_bit_count = "1010" else '0';
	rx_sample <= '1' when rx_baud_count = "0110011" else '0';
	
	process(reset,clock)
	begin
		if reset='0' then
			rx_bit_count <= (others=>'0');
		elsif rising_edge(clock) then
			if rx_active='0' then
				rx_bit_count <= (others=>'0');
			elsif rx_overrun = '0' and rx_baud_count = baud_top then
				rx_bit_count <= rx_bit_count + 1;
			end if;
		end if;
	end process;
	
	process(clock,reset)
	begin
		if reset='0' then
			rx_active <= '0';
		elsif rising_edge(clock) then
			rx_active <= ((rx_stop nor rx_overrun) and rx_active) or not rx_input;
		end if;
	end process;
	
	process(clock,reset)
	begin
		if reset = '0' then
			rx_shift_reg <= (others=>'0');
		elsif rising_edge(clock) then
			if rx_sample='1' and rx_overrun = '0' and rx_bit_count /= "1001" then
				rx_shift_reg <= rx_input & rx_shift_reg(8 downto 1);
			end if;
		end if;
	end process;
	
	process(clock,reset)
	begin
		if reset = '0' then
			rx_data <= (others=>'0');
			rx_ready <= '0';
		elsif rising_edge(clock) then
			if rx_shift_reg(0)='0' and rx_input='1' and rx_stop ='1' then
				rx_data <= rx_shift_reg(8 downto 1);
				rx_ready <= '1';
			elsif rx_read = '1' then
				rx_ready <= '0';
			end if;			
		end if;
	end process;
	
	process(clock,reset)
	begin
		if reset='0' then
			tx_baud_count <= (others=>'0');
			tx_bit_count <= (others=>'0');
			tx_active <= '0';
		elsif rising_edge(clock) then
			if tx_active = '1' then
				if tx_baud_count = baud_top then
					tx_baud_count <= (others=>'0');
					if tx_bit_count = "1001" then
						tx_bit_count <= "0000";		
						tx_active <= '0';
					else
						tx_bit_count <= tx_bit_count + 1;
					end if;
				else
					tx_baud_count <= tx_baud_count + 1;
				end if;
			elsif tx_start = '1' then
				tx_active <= '1';
			end if;
		end if;
	end process;
	
	process(clock,reset)
	begin
		if reset='0' then
			tx_shift_reg <= "111111111";
		elsif rising_edge(clock) then
			if tx_start = '1' then
				tx_shift_reg <= tx_data & '0';
			elsif tx_active = '1' then
				if tx_baud_count = baud_top then
					tx_shift_reg <= '1' & tx_shift_reg(8 downto 1);
				end if;
			else
				tx_shift_reg <= "111111111";
			end if;
		end if;
	end process;
	
end architecture;

-- End of file




