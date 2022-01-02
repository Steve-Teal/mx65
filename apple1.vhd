-------------------------------------------------
--
-- MX65 - 6502 processor core
-- Apple 1 clone using MX65 6502 core
-- Copyright (c) 2022 Steve Teal
--
-------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 

entity apple1 is
  port (
	rx			: in std_logic;
	tx			: out std_logic;
	clock		: in std_logic;
	reset		: in std_logic);
end entity;

architecture rtl of apple1 is

	component uart port (
	rx			: in std_logic;
	tx			: out std_logic;
	clock		: in std_logic;
	reset		: in std_logic;
	rx_ready	: out std_logic;
	rx_read		: in std_logic;
	tx_start	: in std_logic;
	tx_ready	: out std_logic;
	tx_data		: in std_logic_vector(7 downto 0);
	rx_data		: out std_logic_vector(7 downto 0));
	end component;
		
	component mx65 port (
	clock	: in std_logic;
	reset	: in std_logic;	  
	ce		: in std_logic;
	data_in	: in std_logic_vector(7 downto 0);
	data_out: out std_logic_vector(7 downto 0);
	address	: out std_logic_vector(15 downto 0);
	rw		: out std_logic;
	sync	: out std_logic;
	nmi		: in std_logic;
	irq		: in std_logic);
	end component;
	
	component rom port (
	clock:    in std_logic;
    address:  in  std_logic_vector(7 downto 0); 
	cs:       in std_logic;
	data_out: out std_logic_vector(7 downto 0));
	end component;
	
	component ram port (
	clock    : in  std_logic; 
	cs       : in std_logic;
    we       : in  std_logic; 
    address  : in  std_logic_vector(11 downto 0); 
    data_in  : in  std_logic_vector(7 downto 0); 
    data_out : out  std_logic_vector(7 downto 0));
	end component;
	
	signal rx_ready 	: std_logic;
	signal rx_read		: std_logic;
	signal tx_start		: std_logic;
	signal tx_ready		: std_logic;
	signal data_bus		: std_logic_vector(7 downto 0);
	signal address_bus	: std_logic_vector(15 downto 0);
	signal p1			: std_logic;
	signal p2			: std_logic;
	signal mx65_data	: std_logic_vector(7 downto 0);
	signal uart_data	: std_logic_vector(7 downto 0);
	signal rom_data		: std_logic_vector(7 downto 0);
	signal ram_data		: std_logic_vector(7 downto 0);
	signal sync			: std_logic;
	signal rw			: std_logic;
	signal n_reset		: std_logic;
	signal we			: std_logic;
	signal n_tx_ready	: std_logic;
	signal uart_data_in	: std_logic_vector(7 downto 0);
	signal ram_cs       : std_logic;
	signal rom_cs       : std_logic;
	
begin

	u1: uart port map(rx,tx,clock,'1',rx_ready,rx_read,tx_start,tx_ready,uart_data_in,uart_data);
	u2: mx65 port map(clock,n_reset,p1,data_bus,mx65_data,address_bus,rw,sync,'1','1');
	u3: rom port map(clock,address_bus(7 downto 0),rom_cs,rom_data);
	u4: ram port map(clock,ram_cs,we,address_bus(11 downto 0),data_bus,ram_data);
		
	uart_data_in <= '0' & data_bus(6 downto 0);
	rom_cs <= '1' when address_bus(15 downto 8) = X"FF" else '0';
	ram_cs <= '1' when address_bus(15 downto 12) = "0000" else '0';
	we <= not rw;
	n_reset <= not reset;
	n_tx_ready <= not tx_ready;
	rx_read <= '1' when address_bus = X"D010" and p2='1' else '0';
	tx_start <= '1' when address_bus = X"D012" and p2='1' and rw='0' else '0';
	
	data_bus <= 
		mx65_data when rw = '0' else
		rom_data when rom_cs = '1' else 
		ram_data when ram_cs = '1' else
		'1' & uart_data(6 downto 0) when address_bus = X"D010" else 
		(rx_ready & "0000000") when address_bus = X"D011" else
		(n_tx_ready & "0000000") when address_bus = X"D012" else
		"00000000";
		
	process(clock,reset)
	begin
		if reset='0' then
			p1 <= '0';
			p2 <= '0';
		elsif rising_edge(clock) then
			p1 <= not p1;
			p2 <= p1;
		end if;	
	end process;
		
end architecture;

-- End of file

