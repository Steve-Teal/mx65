-------------------------------------------------
--
-- 4K RAM
-- This file is part of the MX65 6502 Project
-- Copyright (c) 2022 Steve Teal
--
-------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram is
    port (
        clock    : in  std_logic; 
		cs       : in std_logic;
        we       : in  std_logic; 
        address  : in  std_logic_vector(11 downto 0); 
        data_in  : in  std_logic_vector(7 downto 0); 
        data_out : out  std_logic_vector(7 downto 0));
end entity;

architecture rtl of ram is

	type ram_type is array(0 to 4095) of std_logic_vector(7 downto 0);
	
	signal ram : ram_type;
begin
	
	process(Clock)
	begin
		if rising_edge(clock) then
			if we = '1' and cs = '1' then
				ram(to_integer(unsigned(address))) <= data_in;
			elsif cs = '1' then
				data_out <= ram(to_integer(unsigned(address)));
			end if;
		end if;
	end process;

end architecture;

-- End of file

