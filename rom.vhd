-------------------------------------------
--  WOZMON ROM
--  Written by Steve Wozniak in 1976
-------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rom is
    port (
        clock:    in std_logic;
        address:  in std_logic_vector(7 downto 0); 
        cs:       in std_logic;
        data_out: out std_logic_vector(7 downto 0));
end entity;

architecture rtl of rom is

    type rom_type is array(0 to 255) of std_logic_vector(7 downto 0);
    signal rom : rom_type := (
        (X"D8"),(X"58"),(X"A0"),(X"7F"),(X"8C"),(X"12"),(X"D0"),(X"A9"),
        (X"A7"),(X"8D"),(X"11"),(X"D0"),(X"8D"),(X"13"),(X"D0"),(X"C9"),
        (X"DF"),(X"F0"),(X"13"),(X"C9"),(X"9B"),(X"F0"),(X"03"),(X"C8"),
        (X"10"),(X"0F"),(X"A9"),(X"DC"),(X"20"),(X"EF"),(X"FF"),(X"A9"),
        (X"8D"),(X"20"),(X"EF"),(X"FF"),(X"A0"),(X"01"),(X"88"),(X"30"),
        (X"F6"),(X"AD"),(X"11"),(X"D0"),(X"10"),(X"FB"),(X"AD"),(X"10"),
        (X"D0"),(X"99"),(X"00"),(X"02"),(X"20"),(X"EF"),(X"FF"),(X"C9"),
        (X"8D"),(X"D0"),(X"D4"),(X"A0"),(X"FF"),(X"A9"),(X"00"),(X"AA"),
        (X"0A"),(X"85"),(X"2B"),(X"C8"),(X"B9"),(X"00"),(X"02"),(X"C9"),
        (X"8D"),(X"F0"),(X"D4"),(X"C9"),(X"AE"),(X"90"),(X"F4"),(X"F0"),
        (X"F0"),(X"C9"),(X"BA"),(X"F0"),(X"EB"),(X"C9"),(X"D2"),(X"F0"),
        (X"3B"),(X"86"),(X"28"),(X"86"),(X"29"),(X"84"),(X"2A"),(X"B9"),
        (X"00"),(X"02"),(X"49"),(X"B0"),(X"C9"),(X"0A"),(X"90"),(X"06"),
        (X"69"),(X"88"),(X"C9"),(X"FA"),(X"90"),(X"11"),(X"0A"),(X"0A"),
        (X"0A"),(X"0A"),(X"A2"),(X"04"),(X"0A"),(X"26"),(X"28"),(X"26"),
        (X"29"),(X"CA"),(X"D0"),(X"F8"),(X"C8"),(X"D0"),(X"E0"),(X"C4"),
        (X"2A"),(X"F0"),(X"97"),(X"24"),(X"2B"),(X"50"),(X"10"),(X"A5"),
        (X"28"),(X"81"),(X"26"),(X"E6"),(X"26"),(X"D0"),(X"B5"),(X"E6"),
        (X"27"),(X"4C"),(X"44"),(X"FF"),(X"6C"),(X"24"),(X"00"),(X"30"),
        (X"2B"),(X"A2"),(X"02"),(X"B5"),(X"27"),(X"95"),(X"25"),(X"95"),
        (X"23"),(X"CA"),(X"D0"),(X"F7"),(X"D0"),(X"14"),(X"A9"),(X"8D"),
        (X"20"),(X"EF"),(X"FF"),(X"A5"),(X"25"),(X"20"),(X"DC"),(X"FF"),
        (X"A5"),(X"24"),(X"20"),(X"DC"),(X"FF"),(X"A9"),(X"BA"),(X"20"),
        (X"EF"),(X"FF"),(X"A9"),(X"A0"),(X"20"),(X"EF"),(X"FF"),(X"A1"),
        (X"24"),(X"20"),(X"DC"),(X"FF"),(X"86"),(X"2B"),(X"A5"),(X"24"),
        (X"C5"),(X"28"),(X"A5"),(X"25"),(X"E5"),(X"29"),(X"B0"),(X"C1"),
        (X"E6"),(X"24"),(X"D0"),(X"02"),(X"E6"),(X"25"),(X"A5"),(X"24"),
        (X"29"),(X"07"),(X"10"),(X"C8"),(X"48"),(X"4A"),(X"4A"),(X"4A"),
        (X"4A"),(X"20"),(X"E5"),(X"FF"),(X"68"),(X"29"),(X"0F"),(X"09"),
        (X"B0"),(X"C9"),(X"BA"),(X"90"),(X"02"),(X"69"),(X"06"),(X"2C"),
        (X"12"),(X"D0"),(X"30"),(X"FB"),(X"8D"),(X"12"),(X"D0"),(X"60"),
        (X"EA"),(X"EA"),(X"00"),(X"0F"),(X"00"),(X"FF"),(X"00"),(X"FF"));

begin

    process(clock)
    begin
        if rising_edge(clock) then
            if cs = '1' then
                data_out <= rom(to_integer(unsigned(address)));
            end if;
        end if;
    end process;

end architecture;

-- End of file
