-------------------------------------------------
--
-- MX65 - 6502 processor core
-- This file is part of the MX65 Project
-- Copyright (c) 2022 Steve Teal
--
-------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
	
entity mx65 is
  port (
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
end entity;

architecture rtl of mx65 is

	signal a : std_logic_vector(7 downto 0);
	signal x : std_logic_vector(7 downto 0);
	signal y : std_logic_vector(7 downto 0);
	signal s : std_logic_vector(7 downto 0);
	signal dl : std_logic_vector(7 downto 0);
	signal ir : std_logic_vector(7 downto 0);
	signal pcl : std_logic_vector(7 downto 0);
	signal pch : std_logic_vector(7 downto 0);
	signal adl : std_logic_vector(7 downto 0);
	signal adh : std_logic_vector(7 downto 0);
	signal c : std_logic;
	signal z : std_logic;
	signal n : std_logic;
	signal v : std_logic;
	signal d : std_logic;
	signal i : std_logic;	
	signal b : std_logic;
	
	type state_type is (T0,T1,BRK2_JSR3,PHA2,PHP2_BRK4,PLX2_RTX2,JSR2,JMPABS2_JSR5,ABS2,
			BR2,IZX2_ZPX2,IZY2_IZX3_JMPIND3,ZPY2,ABX2,ABY2,TREAD,TWRITE,BRK3_JSR4,PLA3,
			PLP3,RTS3_RTI4,RTI3,BR3,IZY3,RTS4_RTI5,JMPIND4,IZX4,BRK5,BRK6,RTS5,TFIX,TMOD);
	
	type control_type is record 
		src_a,src_x,src_y,src_s,src_dl,src_pcl,src_pch,src_p,dst_a,dst_x,dst_y,dst_dl,
        dst_adl,dst_adh,dst_pcl,dst_pch,dst_s,load_dl,load_adl,load_adh,load_pcl,
        load_pch,load_nv,load_czid,update_c,update_z,update_n,update_v,clcsec,clisei,
        cldsed,inc_pc,alu_fn0,alu_fn1,force_carry,carry_enable,alu_invert,alu_dl,alu_sr,
        bcd_add,bcd_sub,write_enable : std_logic;		
		end record;
			
	type decode_type is record
		brk,jsr,rti,rts,php,plp,pha,pla,bita,jmpabs,jmpind,sty,ldy,cpy,cpx,dey,tya,tay,clv,
		iny,inx,clcsec,clisei,cldsed,ora,anda,eor,adc,sta,lda,cmp,sbc,asl,rotl,lsr,rotr,stx,
		sta_stx_sty,ldx,dec,inc,a_shift,txa,txs,tax,tsx,dex,index_y,zp,ab,zp_index,ab_index,
		indirect_x,indirect_y,single_byte,br,read_mod_write : std_logic;
		
		end record;
		
	signal addr_pc, addr_ad, addr_s : std_logic_vector(15 downto 0);
	signal state : state_type;
	signal control : control_type;
	signal decode : decode_type;
	signal pc_add_1 : std_logic_vector(15 downto 0);
	signal flag_mux : std_logic;
	signal carry_in : std_logic;
	signal half_carry : std_logic;
	signal full_carry : std_logic;
	signal carry_out : std_logic;
	signal carry_reg : std_logic; -- Registered ALU carry out
	signal data_mux : std_logic_vector(7 downto 0);
	signal alu_a    : unsigned(7 downto 0);
	signal alu_b    : unsigned(7 downto 0);
	signal adder_hi : unsigned(4 downto 0);
	signal adder_lo : unsigned(4 downto 0);
	signal alu_out  : unsigned(7 downto 0);
	signal bcd_fix  : unsigned(7 downto 0);	
	signal brk_vect : std_logic_vector(14 downto 0);
	signal reset_reg : std_logic;
	signal nmi_reg : std_logic;
	signal nmi_mask : std_logic;	  
	signal nmi_ready : std_logic;
	signal irq_ready : std_logic;
	signal t0_irq_nmi : std_logic;	 
	signal inhibit_pcinc : std_logic;
	signal enable : std_logic;

begin

	data_mux <= (a or (7 downto 0 => not control.src_a)) and
				(x or (7 downto 0 => not control.src_x)) and
				(y or (7 downto 0 => not control.src_y)) and	 
				(s or (7 downto 0 => not control.src_s)) and	 
				(dl or (7 downto 0 => not control.src_dl)) and
				(pcl or (7 downto 0 => not control.src_pcl)) and
				(pch or (7 downto 0 => not control.src_pch)) and
				( n & v & '1' & b & d & i & z & c or (7 downto 0 => not control.src_p));

	data_out <= data_mux;  
	enable <= ce;
	
	brk_vect(14 downto 2) <= "1111111111111";
	brk_vect(1) <= not nmi_reg;
	brk_vect(0) <= not reset_reg;
	
	rw <= not control.write_enable;
	
	sync <='1' when state = T0 else '0';   	
	
	nmi_ready <= not (reset_reg or nmi or nmi_mask);  
	irq_ready <= not (reset_reg or irq or i);
	t0_irq_nmi <= (nmi_ready or irq_ready) when state = T0 else '0';  
	inhibit_pcinc <= t0_irq_nmi or not b;

	process (clock, reset, enable, state)
	begin
		if reset='1' then
			reset_reg <= '1';
			b <= '1';
			nmi_reg <= '0';
			nmi_mask <= '0';
		elsif rising_edge(clock) then
			if enable = '1' and (state=T0 or state=BRK6) then
				b <= not t0_irq_nmi;
				nmi_reg <= nmi_ready;
				nmi_mask <= nmi_ready or (nmi_mask and not nmi);   
				if state=BRK6 then
					reset_reg <= '0';
				end if;
			end if;
		end if;
	end process;
		
	process (clock, reset, enable, state) begin
		if reset = '1' then
			ir <= X"00";	 	
        elsif rising_edge(clock) then
			if enable='1' and state=T0 then
				if reset_reg='1' or t0_irq_nmi='1' then
					ir <= X"00";
				else
					ir <= data_in;
				end if;
			end if;
        end if;
    end process;
	
	with ir(7 downto 6) select flag_mux <=
		n when "00",
		v when "01",
		c when "10",
		z when "11",
		'0' when others;
		
	addr_pc <= pch & pcl;
	addr_ad <= adh & adl;
	addr_s <= X"01" & s;
	
	
	process(clock, reset, enable)
	begin
		if reset = '1' then
			carry_reg <= '0';
		elsif rising_edge(clock) then
			if enable='1' then
				carry_reg <= carry_out;
			end if;
		end if;
	end	process;
	
	---------------------------------------
	--
	-- Instruction Decoder
	--
	---------------------------------------
	
	process(ir) begin
		decode <= (others=>'0');
		
		decode.index_y <= ir(2) nand (ir(1) nand (ir(7) and not ir(6)));
		decode.zp <= ir(2) and (ir(3) nor ir(4));
		decode.ab <= ir(2) and ir(3) and not ir(4);
		decode.zp_index <= ir(2) and ir(4) and not ir(3);
		decode.ab_index <= (ir(2) or ir(0)) and ir(3) and ir(4);
		decode.indirect_x <= ir(0) and not (ir(2) or ir(3) or ir(4));
		decode.indirect_y <= ir(0) and ir(4) and (ir(2) nor ir(3));	
		decode.single_byte <= ir(3) and (ir(0) nor ir(2));		
		decode.br <= ir(4) and not (ir(3) or ir(2) or ir(1) or ir(0));
		decode.sta_stx_sty <= ir(7) and (ir(6) nor ir(5));
		
		if ir(1 downto 0)="00" then
			case(ir(7 downto 2))is
				when "000000" => decode.brk <= '1';
				when "001000" => decode.jsr <= '1';
				when "010000" => decode.rti <= '1';
				when "011000" => decode.rts <= '1';
				when "000010" => decode.php <= '1';
				when "001010" => decode.plp <= '1';
				when "010010" => decode.pha <= '1';
				when "011010" => decode.pla <= '1';				
				when "100010" => decode.dey <= '1';
				when "100110" => decode.tya <= '1';
				when "101010" => decode.tay <= '1';
				when "101110" => decode.clv <= '1';
				when "110010" => decode.iny <= '1';
				when "111010" => decode.inx <= '1';
				when "010011" => decode.jmpabs <= '1';
				when "011011" => decode.jmpind <= '1';			
				when "100001"|"100011"|"100101"|"100111" => decode.sty <='1';
				when "001001"|"001011" => decode.bita <= '1';						
				when "000110"|"001110" => decode.clcsec <= '1';
				when "010110"|"011110" => decode.clisei <= '1';
				when "110110"|"111110" => decode.cldsed <= '1';
				when "101000"|"101001"|"101011"|"101101"|"101111" => decode.ldy <= '1';
				when "110000"|"110001"|"110011" => decode.cpy <= '1';	
				when "111000"|"111001"|"111011" => decode.cpx <= '1';				
				when others => null;
			end case;
		end if;
		
		if ir(0)='1' then
			case(ir(7 downto 5))is
				when "000" => decode.ora <= '1';
				when "001" => decode.anda <= '1';
				when "010" => decode.eor <= '1';
				when "011" => decode.adc <= '1';
				when "100" => decode.sta <= '1';
				when "101" => decode.lda <= '1';
				when "110" => decode.cmp <= '1';
				when "111" => decode.sbc <= '1';
				when others => null;				
			end case;					
		end if;
		
		if ir(1)='1' then
			case(ir(7 downto 5)) is
				when "000" => decode.asl <= '1';
				when "001" => decode.rotl <= '1';
				when "010" => decode.lsr <= '1';
				when "011" => decode.rotr <= '1';
				when "100" => decode.stx <= '1';
				when "101" => decode.ldx <= '1';
				when "110" => decode.dec <= '1';
				when "111" => decode.inc <= '1';
				when others => null;				
			end case;
			decode.read_mod_write <= ir(6) or not ir(7);			
		end if;
		
		if ir(3 downto 1)="101" then
			case(ir(7 downto 4)) is
				when "0000"|"0010"|"0100"|"0110" => decode.a_shift <= '1';
				when "1000" => decode.txa <= '1';
				when "1001" => decode.txs <= '1';
				when "1010" => decode.tax <= '1';
				when "1011" => decode.tsx <= '1';
				when "1100" => decode.dex <= '1';
				when others => null;
			end case;
		end if;
		
	end process;
	
	---------------------------------------
	--
	-- State machine
	--
	---------------------------------------
	
	process (reset, clock, enable) begin
		if reset='1' then
			state <= T0;
		elsif rising_edge(clock) then
			if enable='1' then
				case state is
					when T0 => state <= T1;
					when T1 =>
						if decode.brk = '1' then
							state <= BRK2_JSR3;
						elsif decode.pha = '1' then
							state <= PHA2;
						elsif decode.php = '1' then
							state <= PHP2_BRK4;
						elsif decode.plp = '1' or decode.pla = '1' or decode.rts = '1' or decode.rti = '1' then
							state <= PLX2_RTX2;
						elsif decode.jsr = '1' then
							state <= JSR2;
						elsif decode.jmpabs = '1' then
							state <= JMPABS2_JSR5;
						elsif decode.ab = '1' then
							state <= ABS2;
						elsif decode.br = '1' and flag_mux = ir(5) then
							state <= BR2;
						elsif decode.zp_index = '1' and decode.index_y = '1' then
							state <= ZPY2;
						elsif decode.zp_index = '1' or decode.indirect_x = '1' then
							state <= IZX2_ZPX2;
						elsif decode.ab_index = '1' and decode.index_y = '1' then
							state <= ABY2;
						elsif decode.ab_index = '1' then
							state <= ABX2;
						elsif decode.indirect_y = '1' then
							state <= IZY2_IZX3_JMPIND3;
						elsif decode.zp = '1' and decode.sta_stx_sty = '1' then
							state <= TWRITE;
						elsif decode.zp = '1' then
							state <= TREAD;	
						else
							state <= T0;
						end if;
					when BRK2_JSR3 => state <= BRK3_JSR4;			
					when PHP2_BRK4 =>
						if decode.brk = '1' then
							state <= BRK5;
						else
							state <= T0;
						end if;
					when PLX2_RTX2 =>
						if decode.pla = '1' then
							state <= PLA3;
						elsif decode.plp = '1' then
							state <= PLP3;
						elsif decode.rti = '1' then
							state <= RTI3;
						else
							state <= RTS3_RTI4;
						end if;
					when JSR2 => state <= BRK2_JSR3;				
					when ABS2|IZX2_ZPX2 => 
						if decode.jmpind = '1' or decode.indirect_x = '1' then
							state <= IZY2_IZX3_JMPIND3;
						elsif decode.sta_stx_sty = '1' then
							state <= TWRITE;
						else
							state <= TREAD;
						end if;
					when BR2 =>
						if dl(7) = carry_out then
							state <= T0;
						else
							state <= BR3;
						end if;
					when IZY2_IZX3_JMPIND3 =>
						if decode.indirect_x = '1' then
							state <= IZX4;
						elsif decode.indirect_y = '1' then
							state <= IZY3;
						else
							state <= JMPIND4;
						end if;
					when ZPY2|IZX4 =>
						if decode.sta_stx_sty = '1' then
							state <= TWRITE;
						else
							state <= TREAD;
						end if;
					when ABX2|ABY2|IZY3 => state <= TFIX;
					when TREAD =>
						if decode.read_mod_write = '1' then
							state <= TMOD;
						else
							state <= T0;
						end if;				
					when BRK3_JSR4 =>
						if decode.brk = '1' then
							state <= PHP2_BRK4;
						else
							state <= JMPABS2_JSR5;
						end if;			
					when RTS3_RTI4 => state <= RTS4_RTI5;
					when RTI3 => state <= RTS3_RTI4;
					when RTS4_RTI5 =>
						if decode.rts = '1' then
							state <= RTS5;
						else
							state <= T0;
						end if;				
					when BRK5 => state <= BRK6;							
					when TFIX =>
						if decode.sta_stx_sty = '1' then
							state <= TWRITE;
						elsif decode.read_mod_write = '1' or carry_reg = '1' then
							state <= TREAD;
						else
							state <= T0;
						end if;
					when TMOD => state <= TWRITE;
					when TWRITE|PLA3|PLP3|BR3|JMPIND4|BRK6|RTS5|JMPABS2_JSR5|PHA2 => state <= T0;
				end case;		
			end if;
		end if;
	end process;
	
	---------------------------------------
	--
	-- Control signal logic and address mux
	--
	---------------------------------------
	
	process (state, decode, carry_reg, addr_pc, addr_s, addr_ad, d, data_mux, adl, adh, s, reset_reg, brk_vect)
	begin
		control <= (others => '0');				
		case state is
			when T0 =>		
				control.inc_pc <= '1';
				-- Source
				control.src_a <= decode.tay or decode.tax or decode.cmp or decode.a_shift or decode.ora
					or decode.anda or decode.eor or decode.bita or decode.adc or decode.sbc;
				control.src_x <= decode.inx or decode.dex or decode.txa or decode.txs or decode.cpx;
				control.src_y <= decode.iny or decode.dey or decode.tya or decode.cpy;
				control.src_dl <= decode.pla or decode.lda or decode.ldy or (decode.ldx and not decode.tsx); 
				control.src_s <= decode.tsx;
				-- Destination
				control.dst_a <= decode.pla or decode.a_shift or decode.txa or decode.tya or decode.ora
					or decode.anda or decode.eor or decode.adc or decode.sbc or decode.lda;
				control.dst_x <= decode.inx or decode.dex or decode.tax or decode.tsx or decode.ldx;
				control.dst_y <= decode.iny or decode.dey or decode.tay or decode.ldy;
				control.dst_s <= decode.txs;
				-- Function
				control.bcd_add <= decode.adc and d;
				control.bcd_sub <= decode.sbc and d;
				control.alu_fn0 <= decode.ora or decode.eor;
				control.alu_fn1 <= decode.anda or decode.bita or decode.eor;	
				control.force_carry <= decode.iny or decode.inx or decode.cmp or decode.cpx or decode.cpy;
				control.carry_enable <= decode.adc or decode.sbc or decode.rotl or decode.rotr;
				control.alu_invert <= decode.dey or decode.dex or decode.sbc or decode.cmp or decode.cpx or decode.cpy;
				control.alu_dl <= decode.ora or decode.anda or decode.eor or decode.bita or decode.adc or
					decode.sbc or decode.cmp or decode.cpx or decode.cpy or decode.asl or decode.rotl;
				control.alu_sr <= decode.lsr or decode.rotr;
				-- Flags   
				
				control.update_n <= decode.pla or decode.a_shift or decode.txa or decode.tya or decode.ora
					or decode.anda or decode.eor or decode.adc or decode.sbc or decode.lda or decode.inx or
					decode.dex or decode.tax or decode.cpx or decode.tsx or decode.iny or decode.ldx or
					decode.dey or decode.tay or decode.ldy or decode.cmp or decode.cpx or decode.cpy;	   
				
				control.update_z <= decode.pla or decode.a_shift or decode.txa or decode.tya or decode.ora
					or decode.anda or decode.eor or decode.adc or decode.sbc or decode.lda or decode.inx or
					decode.dex or decode.tax or decode.cpx or decode.tsx or decode.iny or decode.ldx or
					decode.dey or decode.tay or decode.ldy or decode.cmp or decode.cpx or decode.cpy or decode.bita;   
				
				control.update_c <= decode.adc or decode.sbc or decode.cmp or decode.cpx or decode.cpy or decode.a_shift;
				control.update_v <= decode.adc or decode.sbc or decode.clv;
				control.clcsec <= decode.clcsec;
				control.clisei <= decode.clisei;
				control.cldsed <= decode.cldsed;
				-- Address
				address <= addr_pc;	 
				alu_a <= unsigned(data_mux);
			when T1 => -- Single Byte: DL = A   Multi Byte: DL,ADL = [PC++], ADH = 0				
				control.dst_dl <= decode.single_byte;
				control.src_a <= decode.single_byte;				
				control.load_dl <= not decode.single_byte;
				control.load_adl <= not decode.single_byte;
				control.dst_adh <= not decode.single_byte;
				control.force_carry <= not decode.single_byte; -- ALU output = 0 (0xFF + 1)
				control.inc_pc <= not decode.single_byte;								
				address <= addr_pc;		 
				alu_a <= unsigned(data_mux);					
			when BRK2_JSR3 => -- [S--] = PCH
				control.write_enable <= not reset_reg;
				control.src_pch <= '1';
				control.alu_invert <= '1'; 
				control.dst_s <= '1';
				address <= addr_s;	
				alu_a <= unsigned(s);
			when PHA2 => -- [S--] = A
				control.write_enable <= '1';
				control.src_a <= '1';
				control.alu_invert <= '1';
				control.dst_s <= '1';
				address <= addr_s;		
				alu_a <= unsigned(s);
			when PHP2_BRK4 => -- [S--] = P
				control.write_enable <= not reset_reg;
				control.src_p <= '1';
				control.alu_invert <= '1';
				control.dst_s <= '1';
				address <= addr_s; 
				alu_a <= unsigned(s);
			when PLX2_RTX2 => -- [S++]
				control.force_carry <= '1';
				control.dst_s <= '1';
				address <= addr_s;
				alu_a <= unsigned(s);
			when JSR2 => -- [S]
				address <= addr_s;	
				alu_a <= unsigned(data_mux);
			when JMPABS2_JSR5 => -- PCH=[PC]; PCL=ADL
				control.load_pch <= '1';				
				control.dst_pcl <= '1';
				address <= addr_pc;	
				alu_a <= unsigned(adl);
			when ABS2 => -- ADH = [PC++]
				control.load_adh <= '1';
				control.inc_pc <= '1';
				address <= addr_pc;	  
				alu_a <= unsigned(data_mux);
			when BR2 => -- PCL=PCL+DL; [PC]
				control.src_pcl <= '1';
				control.alu_dl <= '1';
				control.dst_pcl <= '1';
				address <= addr_pc;	
				alu_a <= unsigned(data_mux);
			when IZX2_ZPX2 => -- ADL=X+DL; [AD]
				control.src_x <= '1';
				control.alu_dl <= '1';
				control.dst_adl <= '1';				
				address <= addr_ad;	
				alu_a <= unsigned(data_mux);
			when IZY2_IZX3_JMPIND3 => -- DL=[ADL++]
				control.load_dl <= '1';
				control.dst_adl <= '1';
				control.force_carry <= '1';	
				address <= addr_ad;	
				alu_a <= unsigned(adl);
			when ZPY2 => -- ADL=Y+DL; [AD]
				control.src_y <= '1';
				control.alu_dl <= '1';
				control.dst_adl <= '1';
				address <= addr_ad;	 
				alu_a <= unsigned(data_mux);
			when ABX2 => -- ADH=[PC++]; ADL=X+DL
				control.src_x <= '1';
				control.dst_adl <= '1';
				control.alu_dl <= '1';
				control.load_adh <= '1';
				control.inc_pc <= '1';
				address <= addr_pc;	 
				alu_a <= unsigned(data_mux);
			when ABY2 => -- ADH=[PC++]; ADL=Y+DL
				control.src_y <= '1';
				control.dst_adl <= '1';
				control.alu_dl <= '1';
				control.load_adh <= '1';
				control.inc_pc <= '1';
				address <= addr_pc;	
				alu_a <= unsigned(data_mux);
			when TREAD => -- DL = [AD]
				control.load_dl <= '1';
				control.load_nv <= decode.bita;
				address <= addr_ad;	
				alu_a <= unsigned(data_mux);
			when TWRITE => -- [AD] = reg
				control.src_a <= decode.sta;
				control.src_x <= decode.stx;
				control.src_y <= decode.sty;
				control.src_dl <= decode.read_mod_write;
				control.write_enable <= '1';
				address <= addr_ad;	
				alu_a <= unsigned(data_mux);
			when BRK3_JSR4 => -- [S--] = PCL
				control.write_enable <= not reset_reg;
				control.alu_invert <= '1';
				control.dst_s <= '1';
				control.src_pcl <= '1';
				address <= addr_s; 
				alu_a <= unsigned(s);
			when PLA3 => -- DL=[S]
				control.load_dl <= '1';
				address <= addr_s;
				alu_a <= unsigned(data_mux);
			when PLP3 => -- P=[S]
				control.load_czid <= '1';
				control.load_nv <= '1';
				address <= addr_s; 
				alu_a <= unsigned(data_mux);
			when RTS3_RTI4 => -- PCL=[S++]
				control.force_carry <= '1';
				control.dst_s <= '1';
				control.load_pcl <= '1';
				address <= addr_s;	
				alu_a <= unsigned(s);
			when RTI3 => -- P=[S++]
				control.force_carry <= '1';
				control.dst_s <= '1';
				control.load_czid <= '1';
				control.load_nv <= '1';
				address <= addr_s;
				alu_a <= unsigned(s);
			when BR3 =>	-- if carry_reg) PCH++; else PCH--;
				control.force_carry <= carry_reg;
				control.alu_invert <= not carry_reg;				
				control.src_pch <= '1';
				control.dst_pch <= '1';
				address <= addr_pc;	
				alu_a <= unsigned(data_mux);
			when IZY3 => -- ADH=[AD]; ADL=DL+Y
				control.src_y <= '1';
				control.dst_adl <= '1';
				control.alu_dl <= '1';
				control.load_adh <= '1';
				address <= addr_ad;
				alu_a <= unsigned(data_mux);
			when RTS4_RTI5 => -- PCH=[S]
				control.load_pch <= '1';
				address <= addr_s;
				alu_a <= unsigned(data_mux);
			when JMPIND4 => -- PCH=[AD]; PCL=DL
				control.load_pch <= '1';
				control.src_dl <= '1';
				control.dst_pcl <= '1';
				address <= addr_ad;	
				alu_a <= unsigned(data_mux);
			when IZX4 => -- ADH = [AD]; ADL=DL
				control.load_adh <= '1';
				control.src_dl <= '1';
				control.dst_adl <= '1';
				address <= addr_ad;
				alu_a <= unsigned(data_mux);
			when BRK5 => -- PCL = [VECTOR_LOW]
				control.load_pcl <= '1';
				address <= brk_vect & '0'; 
				alu_a <= unsigned(data_mux);
			when BRK6 => -- PCH = [VECTOR_HIGH]
				control.load_pch <= '1';
				address <= brk_vect & '1';
				alu_a <= unsigned(data_mux);
			when RTS5 => -- [PC++]
				control.inc_pc <= '1';
				address <= addr_pc;
				alu_a <= unsigned(data_mux);
			when TFIX => -- DL = [AD], page boundry crossed: ADH++
				control.dst_adh <= carry_reg;				
				control.force_carry <= '1';
				control.load_dl <= '1';
				address <= addr_ad;
				alu_a <= unsigned(adh);
			when TMOD =>	
				control.write_enable <= '1';
				control.src_dl <= '1';
				control.dst_dl <= '1';
				control.alu_sr <= decode.rotr or decode.lsr;
				control.alu_dl <= decode.rotl or decode.asl;
				control.carry_enable <= decode.rotr or decode.rotl;
				control.force_carry <= decode.inc;
				control.alu_invert <= decode.dec;
				control.update_z <= '1';
				control.update_n <= '1';
				control.update_c <= decode.rotl or decode.asl or decode.rotr or decode.lsr;
				address <= addr_ad;	 
				alu_a <= unsigned(data_mux);
		end case;
	end process;

	---------------------------------------
	--
	-- DL Register
	--
	---------------------------------------		
	
	process(clock, reset, enable)
	begin
		if reset = '1' then
			dl <= X"00";	
		elsif rising_edge(clock) then
			if enable='1' then
				if control.load_dl = '1' then
					dl <= data_in;
				elsif control.dst_dl = '1' then
					dl <= std_logic_vector(alu_out);
				end if;
			end if;
		end if;
	end process;
	
	
	---------------------------------------
	--
	-- ADL Register
	--
	---------------------------------------	

	process(clock, reset, enable)
	begin
		if reset = '1' then
			adl <= (others=>'1');
		elsif rising_edge(clock) then
			if enable='1' then
				if control.load_adl = '1' then
					adl <= data_in;
				elsif control.dst_adl = '1' then
					adl <= std_logic_vector(alu_out);
				end if;
			end if;
		end if;
	end process;


	---------------------------------------
	--
	-- ADH Register
	--
	---------------------------------------

	process(clock, reset, enable)
	begin
		if reset = '1' then
			adh <= (others=>'0');
		elsif rising_edge(clock) then
			if enable='1' then	
				if control.load_adh = '1' then
					adh <= data_in;	 
				elsif control.dst_adh = '1' then
					adh <= std_logic_vector(alu_out);
				end if;
			end if;
		end if;		
	end process;
	
	---------------------------------------
	--
	-- A,X,Y,S Registers
	--
	---------------------------------------

	process(clock, reset, enable)
	begin
		if reset = '1' then
			a <= (others=>'0');
			x <= (others=>'0');
			y <= (others=>'0');	   
			s <= (others=>'0');
		elsif rising_edge(clock) then
			if enable='1' then
				if control.dst_a = '1' then
					a(3 downto 0) <= std_logic_vector(alu_out(3 downto 0) + bcd_fix(3 downto 0));
					a(7 downto 4) <= std_logic_vector(alu_out(7 downto 4) + bcd_fix(7 downto 4));
				end if;
				if control.dst_x = '1' then
					x <= std_logic_vector(alu_out);
				end if;
				if control.dst_y = '1' then
					y <= std_logic_vector(alu_out);
				end if;
				if control.dst_s = '1' then
					s <= std_logic_vector(alu_out);
				end if;
			end if;
		end if;
	end process;
	
	---------------------------------------
	--
	-- PCH, PCL registers
	--
	---------------------------------------
	
	process(clock, reset, addr_pc, enable)
	begin
		pc_add_1 <= std_logic_vector(unsigned(addr_pc) + 1);
		if reset = '1' then
			pcl <= (others=>'0');
			pch <= (others=>'0');
		elsif rising_edge(clock) then
			if enable='1' then
				if control.inc_pc = '1' and inhibit_pcinc = '0' then
					pcl <= pc_add_1(7 downto 0);
					pch <= pc_add_1(15 downto 8);
				else
					if control.load_pcl = '1' then
						pcl <= data_in;
					elsif control.dst_pcl = '1' then
						pcl <= std_logic_vector(alu_out);
					end if;
					if control.load_pch = '1' then
						pch <= data_in;
					elsif control.dst_pch = '1' then
						pch <= std_logic_vector(alu_out);
					end if;
				end if;
			end if;
		end if;
	end process;

	---------------------------------------
	--
	-- Carry Flag
	--
	---------------------------------------
    
	process(clock, reset, enable)
	begin
		if reset = '1' then
			c <= '0';			
		elsif rising_edge(clock) then
			if enable='1' then
				if control.load_czid = '1' then
					c <= data_in(0);
				elsif control.update_c = '1' then
					c <= carry_out;
				elsif control.clcsec = '1' then
					c <= ir(5);
				end if;
			end if;
		end if;
	end process;
	
	---------------------------------------
	--
	-- Zero Flag
	--
	---------------------------------------
    
	process(clock, reset, enable)
	begin	
		if reset = '1' then
			z <= '0';
		elsif rising_edge(clock) then
			if enable='1' then
				if control.load_czid = '1' then
					z <= data_in(1);
				elsif control.update_z = '1' then		
					if alu_out = X"00" then
						z <= '1';
					else
						z <= '0';
					end if;
				end if;			
			end if;
		end if;
	end process;
	
	---------------------------------------
	--
	-- Interrupt Disable Flag
	--
	---------------------------------------
    
	process(clock, reset, enable)
	begin	
		if reset = '1' then
			i <= '1'; -- Interrupts disabled
		elsif rising_edge(clock) then
			if enable='1' then
				if control.load_czid = '1' then
					i <= data_in(2);
				elsif control.clisei = '1' then
					i <= ir(5);
				elsif state = BRK6 then	
					i <= '1';
				end if;
			end if;
		end if;
	end process;
	
	---------------------------------------
	--
	-- Decimal Flag
	--
	---------------------------------------
    
	process(clock, reset, enable)
	begin	
		if reset = '1' then
			d <= '0'; 
		elsif rising_edge(clock) then
			if enable='1' then	
				if control.load_czid = '1' then
					d <= data_in(3);
				elsif control.cldsed = '1' then
					d <= ir(5);
				end if;
			end if;
		end if;
	end process;
	
	---------------------------------------
	--
	-- Overflow Flag
	--
	---------------------------------------
    
	process(clock, reset, enable)
	begin	
		if reset = '1' then
			v <= '0'; 
		elsif rising_edge(clock) then
			if enable='1' then
				if control.load_nv = '1' then
					v <= data_in(6);
				elsif control.update_v = '1' then
					-- Overflow occurs when the ALU input signs match and the output sign does not
					if alu_a(7) /= adder_hi(3) and alu_b(7) /= adder_hi(3) then
						v <= '1';
					else
						v <= '0';
					end if;			
				end if;
			end if;
		end if;
	end process;
	
	---------------------------------------
	--
	-- Negative Flag
	--
	---------------------------------------
    
	process(clock, reset, enable)
	begin	
		if reset = '1' then
			n <= '0'; 	
		elsif rising_edge(clock) then
			if enable='1' then
				if control.load_nv = '1' then
					n <= data_in(7);
				elsif control.update_n = '1' then
					n <= alu_out(7);
				end if;
			end if;
		end if;
	end process;
	
	---------------------------------------
	--
	-- ALU B input
	--
	---------------------------------------	
	
	process(control, dl, enable)
	begin
		if control.alu_dl = '1' then
			if control.alu_invert = '1' then
				alu_b <= unsigned(not dl);
			else
				alu_b <= unsigned(dl);
			end if;
		else
			if control.alu_invert = '1' then
				alu_b <= X"FF";
			else
				alu_b <= X"00";
			end if;
		end if;
	end process;

	---------------------------------------
	--
	-- ALU 
	--
	---------------------------------------	

	carry_in <= control.force_carry or (control.carry_enable and c);
	adder_lo <= ('0' & alu_a(3 downto 0)) + ('0' & alu_b(3 downto 0)) + ("0000" & carry_in);
	adder_hi <= ('0' & alu_a(7 downto 4)) + ('0' & alu_b(7 downto 4)) + ("0000" & half_carry);
	half_carry <= adder_lo(4) or (control.bcd_add and adder_lo(3) and (adder_lo(2) or adder_lo(1)));
	full_carry <= adder_hi(4) or (control.bcd_add and adder_hi(3) and (adder_hi(2) or adder_hi(1)));
	
	process(control,carry_in,alu_a,alu_b,adder_hi,adder_lo,full_carry)
	begin
		if control.alu_sr = '1' then
			alu_out <= (carry_in & alu_a(7 downto 1));
			carry_out <= alu_a(0);
		else
			if control.alu_fn1 = '0' and control.alu_fn0 = '0' then
				alu_out <= (adder_hi(3 downto 0) & adder_lo(3 downto 0));
			elsif control.alu_fn1 = '0' and control.alu_fn0 = '1' then
				alu_out <= alu_a or alu_b;
			elsif control.alu_fn1 = '1' and control.alu_fn0 = '0' then
				alu_out <= alu_a and alu_b;
			else
				alu_out <= alu_a xor alu_b;
			end if;
			carry_out <= full_carry;
		end if;
	end process;
	

	---------------------------------------
	--
	-- BCD FiX 
	--
	---------------------------------------		
	
	bcd_fix(0) <= '0';
	bcd_fix(1) <= bcd_fix(2) or bcd_fix(3);
	bcd_fix(2) <= control.bcd_add and half_carry;
	bcd_fix(3) <= control.bcd_sub and not half_carry;
	bcd_fix(4) <= '0';
	bcd_fix(5) <= bcd_fix(6) or bcd_fix(7);
	bcd_fix(6) <= control.bcd_add and full_carry;
	bcd_fix(7) <= control.bcd_sub and not full_carry;
		
end architecture;

-- End of file

