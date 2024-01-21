#define __ASSEMBLY__
#include <linux/linkage.h>
#include "defs.H"

	/* ********************************************************************
	 *  Apple II emulator by Alexander Jean-Claude Bottema (C) 1994
	 *
	 * ***************************************************************** */

	/* --------------------------------------------------------------------
	   ....................................................................
	    Memory documentation
	  
	    I use two jump tables; one for get_memory and one for set_memory
	    respectively. The jump tables contain exactly 64k entries to
            avoid unnecessary range checking. Furthermore, special access
	    or to serve memory mapped I/O services are easily obtained by
	    patching the jump table with various routines.
	  
	    Apple 64k address space:
	  
	    (Two byte addresses are represented with least significant
	     byte first, e.g. address FA59 is represented as 59 FA)
	  
	    Address		Description
	  
	    0000 - 00FF   	Zero page RAM
	    0100 - 01FF		Stack
	    0200 - 03EF		RAM
	    03F0 - 03F1		Address for BRK instruction
	                        (normally 59 FA = FA59)
	    03F2 - 03F3         Soft entry vector (normally BF 9D = 9DBF)
	    03F4                Power-up byte
	    03F5 - 03F7         Jump instruction to the subroutine which
	                        handles Applesoft II &-commands
	   			(normally 4C 58 FF = JMP FF58)
	    03F8 - 03FA		Jump instruction to the subroutine which
	                        handles User CTRL-Y commands
	                        (normally 4C 65 FF = JMP FF65)
	    03FB - 03FD		Jump instruction to the subroutine which
	                        handles Non-Maskable Interrupts (NMI)
	                        (normally 4C 65 FF = JMP FF65)
	    03FE - 03FF         Address of the subroutine which handles
	                        Interrupt ReQuests (IRQ)
	                        (normally 65 FF = FF65)
	    0400 - 07FF         Basically primary video text page
	    0478 - 047F		I/O Scratchpad RAM Addresses for Slot 0 - 7
	    04F8 - 04FF		               - " " -
	    0578 - 057F		               - " " -
	    05F8 - 05FF		               - " " -
	    0678 - 067F		               - " " -
	    06F8 - 06FF		               - " " -
	    0778 - 077F		               - " " -
	    07F8 - 07FF		               - " " -
	    ----- These two addresses are pretty strange; the values
	          provided were inconsistent at least on my Apple II
	    05F8		Holds the slot number of the disk
	  			controller card from which DOS was
	                        booted.
	    07F8		Holds the slot number (CX, X = Slot #)
	                        of the slot that is currently active.
	  
	    0800 - 0BFF		Basically secondary video text page
	  
	    0C00 - 1FFF		Plain RAM
	    2000 - 3FFF		Primary video Hi-res page (RAM)
	    4000 - 5FFF         Secondary video Hi-Res page (RAM)
	    6000 - BFFF		Plain RAM
	                        (Normally the operating system is
	                         loaded into ~= 9C00 - BFFF)
	    C000 - C00F		Keyboard data
	                        (C00X contains the character ASCII
	                         code of the pressed key. The
	                         value is available at any C00X
	                         address)
	    C010 - C01F         Clear Keyboard strobe
	    C020 - C02F         Cassette output toggle
	    C030 - C03F         Speaker toggle
	    C040 - C04F         Utility strobe
	    C050		Set graphics mode
	    C051		Set text mode
	    C052		Set all text or graphics
	    C053		Mix text and graphics
	    C054		Display primary page
	    C055		Display secondary page
	    C056		Display low-res graphics
	    C057		Display hi-res graphics
	    C058 - C05F		Annunciator outputs
	    C060		Cassette input
	    C061 - C063		Pushbutton inputs (button 1, 2 and 3)
	    C064 - C067		Game controller inputs
	    C068 - C06F		Same as C060 - C067
	    C070 - C07F		Game controller strobe
	    C080 - C08F		Slot 0 I/O space (usually a language card)
	                        --- If language card
	    C080		    Reset language card
	                            Read mode enabled
	  			    Write mode disabled
	                            Read from language card
	  			    Write to ROM (impossible of course)
	    C081		--- First access
	  			    Read mode disabled
	                            Read from ROM
	  			--- On second access
	                            Write mode enabled
	  			    Write to language card
	    C082		--- (Disable language card)
	                            Read mode disabled
	                            Write mode disabled
	                            Read from ROM
	                            Write to ROM
	    C083		--- First access
	                            Read mode enabled
	  			    Read from language card
	  		 	--- On second access
	  			    Write mode enabled
	  			    Write to language card
	    C088 - C08B		Same as C080 - C083, but
	  			switch to second bank, i.e.
	  			map addresses D000-DFFF to
	  			other 4k area.
	    C100 - C1FF		Slot 1 PROM
	    C200 - C2FF		Slot 2 PROM
	    C300 - C3FF		Slot 3 PROM
	    C400 - C4FF		Slot 4 PROM
	    C500 - C5FF		Slot 5 PROM
	    C600 - C6FF		Slot 6 PROM
	    C700 - C7FF		Slot 7 PROM
	    C800 - CFFF		Expansion ROM (for peripheral cards)
	    CFFF		Disable access to expansion ROM for
	  			ALL peripheral cards.
	    D000 - DFFF		ROM or 4k RAM if language card is
	  			enabled. However, there are TWO 4k
	  			banks that can be mapped onto addresses
	  			D000 - DFFF. See C088 - C08B.
	    E000 - FFFF		ROM or 8k RAM if language card is
	  			enabled.
	   ----------------------------------------------------------------- */

	.align 4
table_opcodes:
	.long		op_BRK			# 00
	.long		op_ORA_ind_x		# 01
	.long		op_HANG			# 02
	.long		op_LOR_ind_x		# 03
	.long		op_NOP_2		# 04
	.long		op_ORA_zpage		# 05
	.long		op_ASL_zpage		# 06
	.long		op_LOR_zpage		# 07
	.long		op_PHP			# 08
	.long		op_ORA_imm		# 09
	.long		op_ASL_acc		# 0A
	.long		op_ANA_imm		# 0B
	.long		op_NOP_3		# 0C
	.long		op_ORA_abs		# 0D
	.long		op_ASL_abs		# 0E
	.long		op_LOR_abs		# 0F
	.long		op_BPL			# 10
	.long		op_ORA_ind_y		# 11
	.long		op_HANG			# 12
	.long		op_LOR_ind_y		# 13
	.long		op_NOP_2		# 14
	.long		op_ORA_zpage_x		# 15
	.long		op_ASL_zpage_x		# 16
	.long		op_LOR_zpage_x		# 17
	.long		op_CLC			# 18
	.long		op_ORA_abs_y		# 19
	.long		op_NOP			# 1A
	.long		op_LOR_abs_y		# 1B
	.long		op_NOP_3		# 1C
	.long		op_ORA_abs_x		# 1D
	.long		op_ASL_abs_x		# 1E
	.long		op_LOR_abs_x		# 1F
	.long		op_JSR			# 20
	.long		op_AND_ind_x		# 21
	.long		op_HANG			# 22
	.long		op_LAN_ind_x		# 23
	.long		op_BIT_zpage		# 24
	.long		op_AND_zpage		# 25
	.long		op_ROL_zpage		# 26
	.long		op_LAN_zpage		# 27
	.long		op_PLP			# 28
	.long		op_AND_imm		# 29
	.long		op_ROL_acc		# 2A
	.long		op_ANB_imm		# 2B
	.long		op_BIT_abs		# 2C
	.long		op_AND_abs		# 2D
	.long		op_ROL_abs		# 2E
	.long		op_LAN_abs		# 2F
	.long		op_BMI			# 30
	.long		op_AND_ind_y		# 31
	.long		op_HANG			# 32
	.long		op_LAN_ind_y		# 33
	.long		op_NOP_2		# 34
	.long		op_AND_zpage_x		# 35
	.long		op_ROL_zpage_x		# 36
	.long		op_LAN_zpage_x		# 37
	.long		op_SEC			# 38
	.long		op_AND_abs_y		# 39
	.long		op_NOP			# 3A
	.long		op_LAN_abs_y		# 3B
	.long		op_NOP_3		# 3C
	.long		op_AND_abs_x		# 3D
	.long		op_ROL_abs_x		# 3E
	.long		op_LAN_abs_x		# 3F
	.long		op_RTI			# 40
	.long		op_EOR_ind_x		# 41
	.long		op_HANG			# 42
	.long		op_REO_ind_x		# 43
	.long		op_NOP_2		# 44
	.long		op_EOR_zpage		# 45
	.long		op_LSR_zpage		# 46
	.long		op_REO_zpage		# 47
	.long		op_PHA			# 48
	.long		op_EOR_imm		# 49
	.long		op_LSR_acc		# 4A
	.long		op_RAM_imm		# 4B
	.long		op_JMP_abs		# 4C
	.long		op_EOR_abs		# 4D
	.long		op_LSR_abs		# 4E
	.long		op_REO_abs		# 4F
	.long		op_BVC			# 50
	.long		op_EOR_ind_y		# 51
	.long		op_HANG			# 52
	.long		op_REO_ind_y		# 53
	.long		op_NOP_2		# 54
	.long		op_EOR_zpage_x		# 55
	.long		op_LSR_zpage_x		# 56
	.long		op_REO_zpage_x		# 57
	.long		op_CLI			# 58
	.long		op_EOR_abs_y		# 59
	.long		op_NOP			# 5A
	.long		op_REO_abs_y		# 5B
	.long		op_NOP_3		# 5C
	.long		op_EOR_abs_x		# 5D
	.long		op_LSR_abs_x		# 5E
	.long		op_REO_abs_x		# 5F
	.long		op_RTS			# 60
	.long		op_ADC_ind_x		# 61
	.long		op_HANG			# 62
	.long		op_RAD_ind_x		# 63
	.long		op_NOP_2		# 64
	.long		op_ADC_zpage		# 65
	.long		op_ROR_zpage		# 66
	.long		op_RAD_zpage		# 67
	.long		op_PLA			# 68
	.long		op_ADC_imm		# 69
	.long		op_ROR_acc		# 6A
	.long		op_RBM_imm		# 6B
	.long		op_JMP_ind		# 6C
	.long		op_ADC_abs		# 6D
	.long		op_ROR_abs		# 6E
	.long		op_RAD_abs		# 6F
	.long		op_BVS			# 70
	.long		op_ADC_ind_y		# 71
	.long		op_HANG			# 72
	.long		op_RAD_ind_y		# 73
	.long		op_NOP_2		# 74
	.long		op_ADC_zpage_x		# 75
	.long		op_ROR_zpage_x		# 76
	.long		op_RAD_zpage_x		# 77
	.long		op_SEI			# 78
	.long		op_ADC_abs_y		# 79
	.long		op_NOP			# 7A
	.long		op_RAD_abs_y		# 7B
	.long		op_NOP_3		# 7C
	.long		op_ADC_abs_x		# 7D
	.long		op_ROR_abs_x		# 7E
	.long		op_RAD_abs_x		# 7F
	.long		op_NOP_2		# 80
	.long		op_STA_ind_x		# 81
	.long		op_NOP_2		# 82
	.long		op_AAX_ind_x		# 83
	.long		op_STY_zpage		# 84
	.long		op_STA_zpage		# 85
	.long		op_STX_zpage		# 86
	.long		op_AAX_zpage		# 87
	.long		op_DEY			# 88
	.long		op_NOP_2		# 89
	.long		op_TXA			# 8A
	.long		op_XMA_imm		# 8B
	.long		op_STY_abs		# 8C
	.long		op_STA_abs		# 8D
	.long		op_STX_abs		# 8E
	.long		op_AAX_abs		# 8F
	.long		op_BCC			# 90
	.long		op_STA_ind_y		# 91
	.long		op_HANG			# 92
	.long		op_AAX_ind_y		# 93
	.long		op_STY_zpage_x		# 94
	.long		op_STA_zpage_x		# 95
	.long		op_STX_zpage_y		# 96
	.long		op_AAX_zpage_y		# 97
	.long		op_TYA			# 98
	.long		op_STA_abs_y		# 99
	.long		op_TXS			# 9A
	.long		op_AXS_abs_y		# 9B
	.long		op_TEY_abs_x		# 9C
	.long		op_STA_abs_x		# 9D
	.long		op_TEX_abs_y		# 9E
	.long		op_TEA_abs_y		# 9F
	.long		op_LDY_imm		# A0
	.long		op_LDA_ind_x		# A1
	.long		op_LDX_imm		# A2
	.long		op_LAX_ind_x		# A3
	.long		op_LDY_zpage		# A4
	.long		op_LDA_zpage		# A5
	.long		op_LDX_zpage		# A6
	.long		op_LAX_zpage		# A7
	.long		op_TAY			# A8
	.long		op_LDA_imm		# A9
	.long		op_TAX			# AA
	.long		op_AMA_imm		# AB
	.long		op_LDY_abs		# AC
	.long		op_LDA_abs		# AD
	.long		op_LDX_abs		# AE
	.long		op_LAX_abs		# AF
	.long		op_BCS			# B0
	.long		op_LDA_ind_y		# B1
	.long		op_HANG			# B2
	.long		op_LAX_ind_y		# B3
	.long		op_LDY_zpage_x		# B4
	.long		op_LDA_zpage_x		# B5
	.long		op_LDX_zpage_y		# B6
	.long		op_LAX_zpage_y		# B7
	.long		op_CLV			# B8
	.long		op_LDA_abs_y		# B9
	.long		op_TSX			# BA
	.long		op_LAS_abs_y		# BB
	.long		op_LDY_abs_x		# BC
	.long		op_LDA_abs_x		# BD
	.long		op_LDX_abs_y		# BE
	.long		op_LAX_abs_y		# BF
	.long		op_CPY_imm		# C0
	.long		op_CMP_ind_x		# C1
	.long		op_NOP_2		# C2
	.long		op_DCP_ind_x		# C3
	.long		op_CPY_zpage		# C4
	.long		op_CMP_zpage		# C5
	.long		op_DEC_zpage		# C6
	.long		op_DCP_zpage		# C7
	.long		op_INY			# C8
	.long		op_CMP_imm		# C9
	.long		op_DEX			# CA
	.long		op_AXM_imm		# CB
	.long		op_CPY_abs		# CC
	.long		op_CMP_abs		# CD
	.long		op_DEC_abs		# CE
	.long		op_DCP_abs		# CF
	.long		op_BNE			# D0
	.long		op_CMP_ind_y		# D1
	.long		op_HANG			# D2
	.long		op_DCP_ind_y		# D3
	.long		op_NOP_2		# D4
	.long		op_CMP_zpage_x		# D5
	.long		op_DEC_zpage_x		# D6
	.long		op_DCP_zpage_x		# D7
	.long		op_CLD			# D8
	.long		op_CMP_abs_y		# D9
	.long		op_NOP			# DA
	.long		op_DCP_abs_y		# DB
	.long		op_NOP_3		# DC
	.long		op_CMP_abs_x		# DD
	.long		op_DEC_abs_x		# DE
	.long		op_DCP_abs_x		# DF
	.long		op_CPX_imm		# E0
	.long		op_SBC_ind_x		# E1
	.long		op_NOP_2		# E2
	.long		op_ISB_ind_x		# E3
	.long		op_CPX_zpage		# E4
	.long		op_SBC_zpage		# E5
	.long		op_INC_zpage		# E6
	.long		op_ISB_zpage		# E7
	.long		op_INX			# E8
	.long		op_SBC_imm		# E9
	.long		op_NOP			# EA
	.long		op_ZBC_imm		# EB
	.long		op_CPX_abs		# EC
	.long		op_SBC_abs		# ED
	.long		op_INC_abs		# EE
	.long		op_ISB_abs		# EF
	.long		op_BEQ			# F0
	.long		op_SBC_ind_y		# F1
	.long		op_HANG			# F2
	.long		op_ISB_ind_y		# F3
	.long		op_NOP_2		# F4
	.long		op_SBC_zpage_x		# F5
	.long		op_INC_zpage_x		# F6
	.long		op_ISB_zpage_x		# F7
	.long		op_SED			# F8
	.long		op_SBC_abs_y		# F9
	.long		op_NOP			# FA
	.long		op_ISB_abs_y		# FB
	.long		op_NOP_3		# FC
	.long		op_SBC_abs_x		# FD
	.long		op_INC_abs_x		# FE
	.long		op_ISB_abs_x		# FF

	/* --------------------------------------------------------------------
	    Routine	: cpu6502
	   ----------------------------------------------------------------- */

ENTRY(cpu6502)
		xorl	%eax, %eax
		call	set_page
		
		xorl	%eax, %eax		# Clear registers
		xorl	%ebx, %ebx
		xorl	%ecx, %ecx
		xorl	%edx, %edx
		xorl	%esi, %esi
		xorl	%edi, %edi

		movl	$0xFA62, %esi		# For now
		# movl	$0xFF59, %esi
		xorb	X_Reg, X_Reg		# Clear X register
		xorb	Y_Reg, Y_Reg		# Clear Y register
		xorb	A_Reg, A_Reg		# Clear A register
		xorb	F_Reg, F_Reg		# Clear flags
		movl	$0x1FF, SP_Reg		# Stack pointer
		cld				# Clear direction flag

		Continue

   /* ----------------------------------
       ADC instructions
      ---------------------------------- */

op_ADC_imm:
		GetImm
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ADC_imm_dec		# Yes, jump to decimal version
		DoADC_b
		Continue
op_ADC_imm_dec:
		DoADC_d
		Continue

op_ADC_zpage:
		GetZPage
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ADC_zpage_dec	# Yes, jump to decimal version
		DoADC_b
		Continue
op_ADC_zpage_dec:
		DoADC_d
		Continue
op_ADC_zpage_x:
		GetZPage_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ADC_zpage_x_dec	# Yes, jump to decimal version
		DoADC_b
		Continue
op_ADC_zpage_x_dec:
		DoADC_d
		Continue

op_ADC_abs:
		GetAbs
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ADC_abs_dec		# Yes, jump to decimal version
		DoADC_b
		Continue
op_ADC_abs_dec:
		DoADC_d
		Continue

op_ADC_abs_x:
		GetAbs_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ADC_abs_x_dec	# Yes, jump to decimal version
		DoADC_b
		Continue
op_ADC_abs_x_dec:
		DoADC_d
		Continue

op_ADC_abs_y:
		GetAbs_Y
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ADC_abs_y_dec	# Yes, jump to decimal version
		DoADC_b
		Continue
op_ADC_abs_y_dec:
		DoADC_d
		Continue

op_ADC_ind_x:
		GetIndZPage_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ADC_ind_x_dec	# Yes, jump to decimal version
		DoADC_b
		Continue
op_ADC_ind_x_dec:
		DoADC_d
		Continue

op_ADC_ind_y:
		GetIndZPage_Y
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ADC_ind_y_dec	# Yes, jump to decimal version
		DoADC_b
		Continue
op_ADC_ind_y_dec:
		DoADC_d
		Continue

   /* ----------------------------------
       AND instructions
      ---------------------------------- */

op_AND_imm:
		GetImm
		DoAND
		Continue

op_AND_zpage:
		GetZPage
		DoAND
		Continue

op_AND_zpage_x:
		GetZPage_X
		DoAND
		Continue

op_AND_abs:
		GetAbs
		DoAND
		Continue

op_AND_abs_x:
		GetAbs_X
		DoAND
		Continue

op_AND_abs_y:
		GetAbs_Y
		DoAND
		Continue

op_AND_ind_x:
		GetIndZPage_X
		DoAND
		Continue

op_AND_ind_y:
		GetIndZPage_Y
		DoAND
		Continue

   /* ----------------------------------
       ASL instructions
      ---------------------------------- */

op_ASL_acc:
		shlb	$1, A_Reg
		FlagNZC
		Continue

op_ASL_zpage:
		GetZPage
		DoASL
		Continue

op_ASL_zpage_x:
		GetZPage_X
		DoASL
		Continue

op_ASL_abs:
		GetAbs
		DoASL
		Continue

op_ASL_abs_x:
		GetAbs_X
		DoASL
		Continue

   /* ----------------------------------
       BCC instruction
      ---------------------------------- */

op_BCC:
		GetFromPC_B
		bt	C_Flag_Bit, FF_Reg
		jc	op_BCC_not
		cbw
		addw	%ax, PC_Reg
op_BCC_not:
		Continue

   /* ----------------------------------
       BCS instruction
      ---------------------------------- */

op_BCS:
		GetFromPC_B
		bt	C_Flag_Bit, FF_Reg
		jnc	op_BCS_not
		cbw
		addw	%ax, PC_Reg
op_BCS_not:
		Continue

   /* ----------------------------------
       BEQ instruction
      ---------------------------------- */

op_BEQ:
		GetFromPC_B
		bt	Z_Flag_Bit, FF_Reg
		jnc	op_BEQ_not
		cbw
		addw	%ax, PC_Reg
op_BEQ_not:
		Continue

   /* ----------------------------------
       BIT instructions
      ---------------------------------- */

op_BIT_zpage:
		GetZPage
		DoBIT
		Continue

op_BIT_abs:
		GetAbs
		DoBIT
		Continue

   /* ----------------------------------
       BMI instruction
      ---------------------------------- */

op_BMI:
		GetFromPC_B
		bt	N_Flag_Bit, FF_Reg
		jnc	op_BMI_not
		cbw
		addw	%ax, PC_Reg
op_BMI_not:
		Continue

   /* ----------------------------------
       BNE instruction
      ---------------------------------- */

op_BNE:
		GetFromPC_B
		bt	Z_Flag_Bit, FF_Reg
		jc	op_BNE_not
		cbw
		addw	%ax, PC_Reg
op_BNE_not:
		Continue

   /* ----------------------------------
       BPL instruction
      ---------------------------------- */

op_BPL:
		GetFromPC_B
		bt	N_Flag_Bit, FF_Reg
		jc	op_BPL_not
		cbw
		addw	%ax, PC_Reg
op_BPL_not:
		Continue

   /* ----------------------------------
       BRK instruction
      ---------------------------------- */

op_BRK:
		incw	PC_Reg
		movw	PC_Reg, %ax
		Push(%ah)
		Push(%al)
		movb	F_Reg, %al
		bts	B_Flag_Bit, %eax
		bts	X_Flag_Bit, %eax
		xorb	%ah, %ah
		movb	SYMBOL_NAME(table_encode_flags)(,%eax,1), %al
		Push(%al)
		bts	I_Flag_Bit, FF_Reg
		movw	SYMBOL_NAME(apple_ii_64k)+0xFFFE, PC_Reg
		Continue

   /* ----------------------------------
       BVC instruction
      ---------------------------------- */

op_BVC:
		GetFromPC_B
		bt	V_Flag_Bit, FF_Reg
		jc	op_BVC_not
		cbw
		addw	%ax, PC_Reg
op_BVC_not:
		Continue

   /* ----------------------------------
       BVS instruction
      ---------------------------------- */

op_BVS:
		GetFromPC_B
		bt	V_Flag_Bit, FF_Reg
		jnc	op_BVS_not
		cbw
		addw	%ax, PC_Reg
op_BVS_not:
		Continue

   /* ----------------------------------
       CLC instruction
      ---------------------------------- */

op_CLC:
		btr	C_Flag_Bit, FF_Reg
		Continue

   /* ----------------------------------
       CLD instruction
      ---------------------------------- */

op_CLD:
		btr	D_Flag_Bit, FF_Reg
		Continue

   /* ----------------------------------
       CLI instruction
      ---------------------------------- */

op_CLI:
		btr	I_Flag_Bit, FF_Reg
		Continue

   /* ----------------------------------
       CLV instruction
      ---------------------------------- */

op_CLV:
		btr	V_Flag_Bit, FF_Reg
		Continue

   /* ----------------------------------
       CMP instructions
      ---------------------------------- */

op_CMP_imm:
		GetImm
		DoCMP
		Continue

op_CMP_zpage:
		GetZPage
		DoCMP
		Continue

op_CMP_zpage_x:
		GetZPage_X
		DoCMP
		Continue

op_CMP_abs:
		GetAbs
		DoCMP
		Continue

op_CMP_abs_x:
		GetAbs_X
		DoCMP
		Continue

op_CMP_abs_y:
		GetAbs_Y
		DoCMP
		Continue

op_CMP_ind_x:
		GetIndZPage_X
		DoCMP
		Continue

op_CMP_ind_y:
		GetIndZPage_Y
		DoCMP
		Continue

   /* ----------------------------------
       CPX instructions
      ---------------------------------- */

op_CPX_imm:
		GetImm
		DoCPX
		Continue

op_CPX_zpage:
		GetZPage
		DoCPX
		Continue

op_CPX_abs:
		GetAbs
		DoCPX
		Continue

   /* ----------------------------------
       CPY instructions
      ---------------------------------- */

op_CPY_imm:
		GetImm
		DoCPY
		Continue

op_CPY_zpage:
		GetZPage
		DoCPY
		Continue

op_CPY_abs:
		GetAbs
		DoCPY
		Continue

   /* ----------------------------------
       DEC instructions
      ---------------------------------- */

op_DEC_zpage:
		GetZPage
		DoDEC
		Continue

op_DEC_zpage_x:
		GetZPage_X
		DoDEC
		Continue

op_DEC_abs:
		GetAbs
		DoDEC
		Continue

op_DEC_abs_x:
		GetAbs_X
		DoDEC
		Continue

   /* ----------------------------------
       DEX instruction
      ---------------------------------- */

op_DEX:
		decb	X_Reg
		FlagNZ
		Continue

   /* ----------------------------------
       DEY instruction
      ---------------------------------- */

op_DEY:
		decb	Y_Reg
		FlagNZ
		Continue

   /* ----------------------------------
       EOR instructions
      ---------------------------------- */

op_EOR_imm:
		GetImm
		DoEOR
		Continue

op_EOR_zpage:
		GetZPage
		DoEOR
		Continue

op_EOR_zpage_x:
		GetZPage_X
		DoEOR
		Continue

op_EOR_abs:
		GetAbs
		DoEOR
		Continue

op_EOR_abs_x:
		GetAbs_X
		DoEOR
		Continue

op_EOR_abs_y:
		GetAbs_Y
		DoEOR
		Continue

op_EOR_ind_x:
		GetIndZPage_X
		DoEOR
		Continue

op_EOR_ind_y:
		GetIndZPage_Y
		DoEOR
		Continue

   /* ----------------------------------
       INC instructions
      ---------------------------------- */

op_INC_zpage:
		GetZPage
		DoINC
		Continue

op_INC_zpage_x:
		GetZPage_X
		DoINC
		Continue

op_INC_abs:
		GetAbs
		DoINC
		Continue

op_INC_abs_x:
		GetAbs_X
		DoINC
		Continue

   /* ----------------------------------
       INX instruction
      ---------------------------------- */

op_INX:
		incb	X_Reg
		FlagNZ
		Continue

   /* ----------------------------------
       INY instruction
      ---------------------------------- */

op_INY:
		incb	Y_Reg
		FlagNZ
		Continue

   /* ----------------------------------
       JMP instructions
      ---------------------------------- */

op_JMP_abs:
		GetAbs
		DoJMP
		Continue

op_JMP_ind:
		movb	SYMBOL_NAME(apple_ii_64k)(,PC_Reg_E,1), %al
		cmpb	$0xFF, %al
		je	special_case
		incw	PC_Reg
		movb	SYMBOL_NAME(apple_ii_64k)(,PC_Reg_E,1), %ah
		movw	SYMBOL_NAME(apple_ii_64k)(,%eax,1), PC_Reg
		Continue
special_case:
		incw	PC_Reg
		movb	SYMBOL_NAME(apple_ii_64k)(,PC_Reg_E,1), %ah
		pushl	%ebx
		movb	SYMBOL_NAME(apple_ii_64k)(,%eax,1), %bl
		incb	%al
		movb	SYMBOL_NAME(apple_ii_64k)(,%eax,1), %bh
		movw	%bx, PC_Reg
		popl	%ebx
		Continue

   /* ----------------------------------
       JSR instruction
      ---------------------------------- */

op_JSR:
		GetAbs
		DoJSR
		Continue

   /* ----------------------------------
       LDA instructions
      ---------------------------------- */

op_LDA_imm:
		GetImm
		DoLDA
		Continue

op_LDA_zpage:
		GetZPage
		DoLDA
		Continue

op_LDA_zpage_x:
		GetZPage_X
		DoLDA
		Continue

op_LDA_abs:
		GetAbs
		DoLDA
		Continue

op_LDA_abs_x:
		GetAbs_X
		DoLDA
		Continue

op_LDA_abs_y:
		GetAbs_Y
		DoLDA
		Continue

op_LDA_ind_x:
		GetIndZPage_X
		DoLDA
		Continue

op_LDA_ind_y:
		GetIndZPage_Y
		DoLDA
		Continue

   /* ----------------------------------
       LDX instructions
      ---------------------------------- */

op_LDX_imm:
		GetImm
		DoLDX
		Continue

op_LDX_zpage:
		GetZPage
		DoLDX
		Continue

op_LDX_zpage_y:
		GetZPage_Y
		DoLDX
		Continue

op_LDX_abs:
		GetAbs
		DoLDX
		Continue

op_LDX_abs_y:
		GetAbs_Y
		DoLDX
		Continue

   /* ----------------------------------
       LDY instructions
      ---------------------------------- */

op_LDY_imm:
		GetImm
		DoLDY
		Continue

op_LDY_zpage:
		GetZPage
		DoLDY
		Continue

op_LDY_zpage_x:
		GetZPage_X
		DoLDY
		Continue

op_LDY_abs:
		GetAbs
		DoLDY
		Continue

op_LDY_abs_x:
		GetAbs_X
		DoLDY
		Continue

   /* ----------------------------------
       LSR instructions
      ---------------------------------- */

op_LSR_acc:
		shrb	$1, A_Reg
		FlagNZC
		Continue

op_LSR_zpage:
		GetZPage
		DoLSR
		Continue

op_LSR_zpage_x:
		GetZPage_X
		DoLSR
		Continue

op_LSR_abs:
		GetAbs
		DoLSR
		Continue

op_LSR_abs_x:
		GetAbs_X
		DoLSR
		Continue

   /* ----------------------------------
       NOP instruction
      ---------------------------------- */

op_NOP:
		Continue

   /* ----------------------------------
       ORA instructions
      ---------------------------------- */

op_ORA_imm:
		GetImm
		DoORA
		Continue

op_ORA_zpage:
		GetZPage
		DoORA
		Continue

op_ORA_zpage_x:
		GetZPage_X
		DoORA
		Continue

op_ORA_abs:
		GetAbs
		DoORA
		Continue

op_ORA_abs_x:
		GetAbs_X
		DoORA
		Continue

op_ORA_abs_y:
		GetAbs_Y
		DoORA
		Continue

op_ORA_ind_x:
		GetIndZPage_X
		DoORA
		Continue

op_ORA_ind_y:
		GetIndZPage_Y
		DoORA
		Continue

   /* ----------------------------------
       PHA instruction
      ---------------------------------- */

op_PHA:
		Push(A_Reg)
		Continue

   /* ----------------------------------
       PHP instruction
      ---------------------------------- */

op_PHP:
		movb	F_Reg, %al
		bts	X_Flag_Bit, %eax
		xorb	%ah, %ah
		movb	SYMBOL_NAME(table_encode_flags)(,%eax,1), %al
		Push(%al)
		Continue

   /* ----------------------------------
       PLA instruction
      ---------------------------------- */

op_PLA:
		Pop(A_Reg)
		orb	A_Reg, A_Reg
		FlagNZ
		Continue

   /* ----------------------------------
       PLP instruction
      ---------------------------------- */
		
op_PLP:
		Pop(%al)
		btr	X_Flag_Bit, %eax
		xorb	%ah, %ah
		movb	SYMBOL_NAME(table_decode_flags)(,%eax,1), F_Reg
		Continue

   /* ----------------------------------
       ROL instructions
      ---------------------------------- */

op_ROL_acc:
		bt	C_Flag_Bit, FF_Reg
		rclb	$1, A_Reg
		jc	op_ROL_acc_carry
		orb	A_Reg, A_Reg
		clc
		FlagNZC
		Continue
op_ROL_acc_carry:
		orb	A_Reg, A_Reg
		stc
		FlagNZC
		Continue

op_ROL_zpage:
		GetZPage
		DoROL(rol_zpage, Continue)

op_ROL_zpage_x:
		GetZPage_X
		DoROL(rol_zpage_x, Continue)

op_ROL_abs:
		GetAbs
		DoROL(rol_abs, Continue)

op_ROL_abs_x:
		GetAbs_X
		DoROL(rol_abs_x, Continue)

   /* ----------------------------------
       ROR instructions
      ---------------------------------- */

op_ROR_acc:
		bt	C_Flag_Bit, FF_Reg
		rcrb	$1, A_Reg
		jc	op_ROR_acc_carry
		orb	A_Reg, A_Reg
		clc
		FlagNZC
		Continue
op_ROR_acc_carry:
		orb	A_Reg, A_Reg
		stc
		FlagNZC
		Continue

op_ROR_zpage:
		GetZPage
		DoROR(ror_zpage, Continue)

op_ROR_zpage_x:
		GetZPage_X
		DoROR(ror_zpage_x, Continue)

op_ROR_abs:
		GetAbs
		DoROR(ror_abs, Continue)

op_ROR_abs_x:
		GetAbs_X
		DoROR(ror_abs_x, Continue)

   /* ----------------------------------
       RTI instruction
      ---------------------------------- */

op_RTI:
		Pop(%al)
		btr	X_Flag_Bit, %eax
		xorb	%ah, %ah
		movb	SYMBOL_NAME(table_decode_flags)(,%eax,1), %al
		movb	%al, F_Reg
		Pop(%al)
		Pop(%ah)
		movw	%ax, PC_Reg
		Continue

   /* ----------------------------------
       RTS instruction
      ---------------------------------- */

op_RTS:
		Pop(%al)
		Pop(%ah)
		incw	%ax
		movw	%ax, PC_Reg
		Continue

   /* ----------------------------------
       SBC instructions
      ---------------------------------- */

op_SBC_imm:
		GetImm
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_SBC_imm_dec		# Yes, jump to decimal version
		DoSBC_b
		Continue
op_SBC_imm_dec:
		DoSBC_d
		Continue

op_SBC_zpage:
		GetZPage
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_SBC_zpage_dec	# Yes, jump to decimal version
		DoSBC_b
		Continue
op_SBC_zpage_dec:
		DoSBC_d
		Continue

op_SBC_zpage_x:
		GetZPage_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_SBC_zpage_x_dec	# Yes, jump to decimal version
		DoSBC_b
		Continue
op_SBC_zpage_x_dec:
		DoSBC_d
		Continue

op_SBC_abs:
		GetAbs
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_SBC_abs_dec		# Yes, jump to decimal version
		DoSBC_b
		Continue
op_SBC_abs_dec:
		DoSBC_d
		Continue

op_SBC_abs_x:
		GetAbs_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_SBC_abs_x_dec	# Yes, jump to decimal version
		DoSBC_b
		Continue
op_SBC_abs_x_dec:
		DoSBC_d
		Continue

op_SBC_abs_y:
		GetAbs_Y
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_SBC_abs_y_dec	# Yes, jump to decimal version
		DoSBC_b
		Continue
op_SBC_abs_y_dec:
		DoSBC_d
		Continue

op_SBC_ind_x:
		GetIndZPage_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_SBC_ind_x_dec	# Yes, jump to decimal version
		DoSBC_b
		Continue
op_SBC_ind_x_dec:
		DoSBC_d
		Continue

op_SBC_ind_y:
		GetIndZPage_Y
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_SBC_ind_y_dec	# Yes, jump to decimal version
		DoSBC_b
		Continue
op_SBC_ind_y_dec:
		DoSBC_d
		Continue

   /* ----------------------------------
       SEC instruction
      ---------------------------------- */

op_SEC:
		bts	C_Flag_Bit, FF_Reg
		Continue

   /* ----------------------------------
       SED instruction
      ---------------------------------- */

op_SED:
		bts	D_Flag_Bit, FF_Reg
		Continue

   /* ----------------------------------
       SEI instruction
      ---------------------------------- */

op_SEI:
		bts	I_Flag_Bit, FF_Reg
		Continue

   /* ----------------------------------
       STA instructions
      ---------------------------------- */

op_STA_imm:
		GetImm
		DoSTA
		Continue

op_STA_zpage:
		GetZPage
		DoSTA
		Continue

op_STA_zpage_x:
		GetZPage_X
		DoSTA
		Continue

op_STA_abs:
		GetAbs
		DoSTA
		Continue

op_STA_abs_x:
		GetAbs_X
		DoSTA
		Continue

op_STA_abs_y:
		GetAbs_Y
		DoSTA
		Continue

op_STA_ind_x:
		GetIndZPage_X
		DoSTA
		Continue

op_STA_ind_y:
		GetIndZPage_Y
		DoSTA
		Continue

   /* ----------------------------------
       STX instructions
      ---------------------------------- */

op_STX_zpage:
		GetZPage
		DoSTX
		Continue

op_STX_zpage_y:
		GetZPage_Y
		DoSTX
		Continue

op_STX_abs:
		GetAbs
		DoSTX
		Continue

   /* ----------------------------------
       STY instructions
      ---------------------------------- */

op_STY_zpage:
		GetZPage
		DoSTY
		Continue

op_STY_zpage_x:
		GetZPage_X
		DoSTY
		Continue

op_STY_abs:
		GetAbs
		DoSTY
		Continue

   /* ----------------------------------
       TAX instruction
      ---------------------------------- */

op_TAX:
		movb	A_Reg, X_Reg
		orb	X_Reg, X_Reg
		FlagNZ
		Continue

   /* ----------------------------------
       TAY instruction
      ---------------------------------- */

op_TAY:
		movb	A_Reg, Y_Reg
		orb	Y_Reg, Y_Reg
		FlagNZ
		Continue

   /* ----------------------------------
       TSX instruction
      ---------------------------------- */

op_TSX:
		movb	SP_Reg_L, X_Reg
		orb	X_Reg, X_Reg
		FlagNZ
		Continue

   /* ----------------------------------
       TXA instruction
      ---------------------------------- */

op_TXA:
		movb	X_Reg, A_Reg
		orb	A_Reg, A_Reg
		FlagNZ
		Continue

   /* ----------------------------------
       TXS instruction
      ---------------------------------- */

op_TXS:
		movb	X_Reg, SP_Reg_L
		Continue

   /* ----------------------------------
       TYA instruction
      ---------------------------------- */

op_TYA:
		movb	Y_Reg, A_Reg
		orb	A_Reg, A_Reg
		FlagNZ
		Continue

   /* ----------------------------------------------------------------------
       Undocumented 6502 (Illegal instructions)
      ---------------------------------------------------------------------- */

   /* ----------------------------------
       HANG instruction
      ---------------------------------- */

op_HANG:
		decb	PC_Reg
		Continue

   /* ----------------------------------
       NOP_2 instruction
      ---------------------------------- */

op_NOP_2:
		incb	PC_Reg
		Continue

   /* ----------------------------------
       NOP_3 instruction
      ---------------------------------- */

op_NOP_3:
		addw	$2, PC_Reg
		Continue

   /* ----------------------------------
       AAX instructions
      ---------------------------------- */

op_AAX_abs:
		GetAbs
		DoAAX
		Continue

op_AAX_zpage:
		GetZPage
		DoAAX
		Continue

op_AAX_zpage_y:
		GetZPage_Y
		DoAAX
		Continue

op_AAX_ind_x:
		GetIndZPage_X
		DoAAX
		Continue

op_AAX_ind_y:
		GetIndZPage_Y
		DoAAX
		Continue

   /* ----------------------------------
       AMA instruction
      ---------------------------------- */

op_AMA_imm:
		GetImm
		DoAMA
		Continue

   /* ----------------------------------
       ANA instruction
      ---------------------------------- */

op_ANA_imm:
		GetImm
		DoANA(Continue)

   /* ----------------------------------
       ANB instruction
      ---------------------------------- */

op_ANB_imm:
		GetImm
		DoANB(Continue)

   /* ----------------------------------
       AXM instruction
      ---------------------------------- */

op_AXM_imm:
		GetImm
		DoAXM
		Continue

   /* ----------------------------------
       AXS instruction
      ---------------------------------- */

op_AXS_abs_y:
		GetAbs_Y
		DoAXS
		Continue

   /* ----------------------------------
       DCP instructions
      ---------------------------------- */

op_DCP_zpage:
		GetZPage
		DoDCP
		Continue

op_DCP_zpage_x:
		GetZPage_X
		DoDCP
		Continue

op_DCP_abs:
		GetAbs
		DoDCP
		Continue

op_DCP_abs_x:
		GetAbs_X
		DoDCP
		Continue

op_DCP_abs_y:
		GetAbs_Y
		DoDCP
		Continue

op_DCP_ind_x:
		GetIndZPage_X
		DoDCP
		Continue

op_DCP_ind_y:
		GetIndZPage_Y
		DoDCP
		Continue

   /* ----------------------------------
       ISB instructions
      ---------------------------------- */

op_ISB_dec:
		DoISB_d
		Continue

op_ISB_zpage:
		GetZPage
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ISB_dec		# Yes, jump to decimal version
		DoISB_b
		Continue

op_ISB_zpage_x:
		GetZPage_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ISB_dec		# Yes, jump to decimal version
		DoISB_b
		Continue


op_ISB_abs:
		GetAbs
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ISB_dec		# Yes, jump to decimal version
		DoISB_b
		Continue

op_ISB_abs_x:
		GetAbs_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ISB_dec		# Yes, jump to decimal version
		DoISB_b
		Continue

op_ISB_abs_y:
		GetAbs_Y
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ISB_dec		# Yes, jump to decimal version
		DoISB_b
		Continue

op_ISB_ind_x:
		GetIndZPage_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ISB_dec		# Yes, jump to decimal version
		DoISB_b
		Continue

op_ISB_ind_y:
		GetIndZPage_Y
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ISB_dec		# Yes, jump to decimal version
		DoISB_b
		Continue

   /* ----------------------------------
       LAN instructions
      ---------------------------------- */

op_LAN_zpage:
		GetZPage
		DoLAN(Continue)

op_LAN_zpage_x:
		GetZPage_X
		DoLAN(Continue)

op_LAN_abs:
		GetAbs
		DoLAN(Continue)

op_LAN_abs_x:
		GetAbs_X
		DoLAN(Continue)

op_LAN_abs_y:
		GetAbs_Y
		DoLAN(Continue)

op_LAN_ind_x:
		GetIndZPage_X
		DoLAN(Continue)

op_LAN_ind_y:
		GetIndZPage_Y
		DoLAN(Continue)

   /* ----------------------------------
       LAS instruction
      ---------------------------------- */

op_LAS_abs_y:
		GetAbs_Y
		DoLAS
		Continue

   /* ----------------------------------
       LAX instructions
      ---------------------------------- */

op_LAX_zpage:
		GetZPage
		DoLAX
		Continue

op_LAX_zpage_y:
		GetZPage_Y
		DoLAX
		Continue

op_LAX_abs:
		GetAbs
		DoLAX
		Continue

op_LAX_abs_y:
		GetAbs_Y
		DoLAX
		Continue

op_LAX_ind_x:
		GetIndZPage_X
		DoLAX
		Continue

op_LAX_ind_y:
		GetIndZPage_Y
		DoLAX
		Continue

   /* ----------------------------------
       LOR instructions
      ---------------------------------- */

op_LOR_zpage:
		GetZPage
		DoLOR
		Continue

op_LOR_zpage_x:
		GetZPage_X
		DoLOR
		Continue

op_LOR_abs:
		GetAbs
		DoLOR
		Continue

op_LOR_abs_x:
		GetAbs_X
		DoLOR
		Continue

op_LOR_abs_y:
		GetAbs_Y
		DoLOR
		Continue

op_LOR_ind_x:
		GetIndZPage_X
		DoLOR
		Continue

op_LOR_ind_y:
		GetIndZPage_Y
		DoLOR
		Continue

   /* ----------------------------------
       RAD instructions
      ---------------------------------- */

op_RAD_dec:
		DoRAD_d
		Continue

op_RAD_zpage:
		GetZPage
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_RAD_dec		# Yes, jump to decimal version
		DoRAD_b
		Continue

op_RAD_zpage_x:
		GetZPage_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_RAD_dec		# Yes, jump to decimal version
		DoRAD_b
		Continue


op_RAD_abs:
		GetAbs
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_RAD_dec		# Yes, jump to decimal version
		DoRAD_b
		Continue

op_RAD_abs_x:
		GetAbs_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_RAD_dec		# Yes, jump to decimal version
		DoRAD_b
		Continue

op_RAD_abs_y:
		GetAbs_Y
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_RAD_dec		# Yes, jump to decimal version
		DoRAD_b
		Continue

op_RAD_ind_x:
		GetIndZPage_X
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_RAD_dec		# Yes, jump to decimal version
		DoRAD_b
		Continue

op_RAD_ind_y:
		GetIndZPage_Y
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_RAD_dec		# Yes, jump to decimal version
		DoRAD_b
		Continue

   /* ----------------------------------
       RAM instruction
      ---------------------------------- */

op_RAM_imm:
		GetImm
		DoRAM
		Continue

   /* ----------------------------------
       RBM instruction
      ---------------------------------- */

op_RBM_imm:
		GetImm
		DoRBM
		Continue

   /* ----------------------------------
       REO instructions
      ---------------------------------- */

op_REO_zpage:
		GetZPage
		DoREO
		Continue

op_REO_zpage_x:
		GetZPage_X
		DoREO
		Continue

op_REO_abs:
		GetAbs
		DoREO
		Continue

op_REO_abs_x:
		GetAbs_X
		DoREO
		Continue

op_REO_abs_y:
		GetAbs_Y
		DoREO
		Continue

op_REO_ind_x:
		GetIndZPage_X
		DoREO
		Continue

op_REO_ind_y:
		GetIndZPage_Y
		DoREO
		Continue

   /* ----------------------------------
       ZBC instruction
      ---------------------------------- */

op_ZBC_imm:
		GetImm
		bt	D_Flag_Bit, FF_Reg	# Decimal mode?
		jc	op_ZBC_dec		# Yes, jump to decimal version
		DoZBC_b
		Continue
op_ZBC_dec:
		DoZBC_d
		Continue

   /* ----------------------------------
       TEA instruction
      ---------------------------------- */

op_TEA_abs_y:
		GetAbs_Y
		DoTEA
		Continue

   /* ----------------------------------
       TEX instruction
      ---------------------------------- */

op_TEX_abs_y:
		GetAbs_Y
		DoTEX
		Continue

   /* ----------------------------------
       TEY instruction
      ---------------------------------- */

op_TEY_abs_x:
		GetAbs_X
		DoTEY
		Continue

   /* ----------------------------------
       XMA instruction
      ---------------------------------- */

op_XMA_imm:
		GetImm
		DoXMA
		Continue

	/* -----------------------------------------------------------------
	    Routine	: update_video_screen
	    Output	: Update video screen
	    Comment	: Scan through entire video memory and call the
			  partial updating routines.
	   ----------------------------------------------------------------- */

ENTRY(update_video_screen)
		pushal

		call	SYMBOL_NAME(c_wait_menu_buf_empty)
		call	SYMBOL_NAME(c_lock_menu_mode_on)

		xorl	%eax, %eax
		xorl	%edi, %edi

		movl	$23, %ebx
update_rows_0:
		movl	$39, %ecx
		movw	SYMBOL_NAME(video_line_offset)(,%ebx,2), %di
		addw	$0x427, %di
update_columns_0:
		movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al
		cmpb	$19, %bl
		jg	update_mixed_0
		call	SYMBOL_NAME(write_ram_text_page0)
		jmp	update_cont_0
update_mixed_0:
		call	SYMBOL_NAME(write_ram_text_mixed0)
update_cont_0:
		decw	%di
		decb	%cl
		jns	update_columns_0
		decb	%bl
		jns	update_rows_0

		movl	$23, %ebx
update_rows_1:
		movl	$39, %ecx
		movw	SYMBOL_NAME(video_line_offset)(,%ebx,2), %di
		addw	$0x827, %di
update_columns_1:
		movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al
		cmpb	$19, %ebx
		jg	update_mixed_1
		call	SYMBOL_NAME(write_ram_text_page1)
		jmp	update_cont_1
update_mixed_1:
		call	SYMBOL_NAME(write_ram_text_mixed1)
update_cont_1:
		decw	%edi
		decb	%cl
		jns	update_columns_1
		decb	%bl
		jns	update_rows_1

		movl	$191, %ebx
update_hires_rows_0:
		movl	$20, %ecx
		movl	%ebx, %edx
		shrb	$3, %dl
		movw	SYMBOL_NAME(video_line_offset)(,%edx,2), %di
		movl	%ebx, %edx
		andb	$0x7, %dl
		shlw	$10, %dx
		addw	$0x2027, %dx
		addw	%dx, %di
update_hires_columns_0:
		movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al
		cmpw	$159, %bx
		jg	update_hires_mixed_0
		call	SYMBOL_NAME(write_ram_hires_page0_odd)
		decw	%di
		movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al
		call	SYMBOL_NAME(write_ram_hires_page0_even)
		jmp	update_hires_cont_0
update_hires_mixed_0:
		call	SYMBOL_NAME(write_ram_hires_mixed0_odd)
		decw	%di
		movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al
		call	SYMBOL_NAME(write_ram_hires_mixed0_even)
update_hires_cont_0:
		decw	%di
		decb	%cl
		jnz	update_hires_columns_0
		decw	%bx
		jns	update_hires_rows_0

		movl	$191, %ebx
update_hires_rows_1:
		movl	$20, %ecx
		movl	%ebx, %edx
		shrb	$3, %dl
		movw	SYMBOL_NAME(video_line_offset)(,%edx,2), %di
		movl	%ebx, %edx
		andb	$0x7, %dl
		shlw	$10, %dx
		addw	$0x4027, %dx
		addw	%dx, %di
update_hires_columns_1:
		movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al
		cmpw	$159, %bx
		jg	update_hires_mixed_1
		call	SYMBOL_NAME(write_ram_hires_page1_odd)
		decw	%di
		movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al
		call	SYMBOL_NAME(write_ram_hires_page1_even)
		jmp	update_hires_cont_1
update_hires_mixed_1:
		call	SYMBOL_NAME(write_ram_hires_mixed1_odd)
		decw	%di
		movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al
		call	SYMBOL_NAME(write_ram_hires_mixed1_even)
update_hires_cont_1:
		decw	%di
		decb	%cl
		jnz	update_hires_columns_1
		decw	%bx
		jns	update_hires_rows_1

		call	SYMBOL_NAME(c_lock_menu_mode_off)

		popal
		ret

	/* --------------------------------------------------------------------
	    Routine	: set_page
	    Input	: AL = Page number (0 or 1)
	    Output	: Current active page bit in video register is set
			  appropriately. EAX destroyed.
	    Usage	: call set_page (with appropriate content in AL).
	    Comment	: The set_page routine sets the appropriate 64K SVGA
			  video write page and updates GM (Graph Memory base
			  address).
	   ----------------------------------------------------------------- */

		.align 4
set_page:
		orb	%al, %al		# Check content of AL
		movb	%al, SYMBOL_NAME(vmode_active)
		jnz	set_page_1		# AL = 1 ?
set_page_0:
		pushal
		pushl	$0
set_page_01:	call	SYMBOL_NAME(c_setpage)
		addl	$4, %esp
		popal
		ret
set_page_1:
		pushal
		pushl	$1
		jmp	set_page_01

	/* --------------------------------------------------------------------
	    Routine	: read_memory
	    Input	: EffectiveAddr (Currently EDI)
	    Output	: AL = Byte value at 6502 address EffectiveAddr
	  		  All other registers are unaffected
	    Usage	: call get_memory (with appropriate content in ESI)
	    Comment	: The get_memory routine uses a jump table (thus
	                  avoiding range checking of particular address areas)
	                  to jump to the appropriate routine given the address.
	  		  Hence, the jump table is consisted of 64k addresses
			  (this consumes 256k of memory).
	   ----------------------------------------------------------------- */
		.align 4
read_memory:
		jmp	* SYMBOL_NAME(table_read_memory)(,EffectiveAddr_E,4)

	/* --------------------------------------------------------------------
	    Routine	: set_memory
	    Input	: EffectiveAddr (Currently EDI)
	    Output	: AL = Byte value
	  		  All other registers are unaffected
	    Usage	: call set_memory (with appropriate content in EDI and
			  AL)
	    Comment	: The set_memory routine uses a jump table (thus
	                  avoiding range checking of particular address areas)
	                  to jump to the appropriate routine given the address.
	  		  Hence, the jump table is consisted of 64k addresses
			  (this consumes 256k of memory).
	   ----------------------------------------------------------------- */
		.align 4
write_memory:
		jmp	* SYMBOL_NAME(table_write_memory)(,EffectiveAddr_E,4)

ENTRY(ram_nop)
		ret

ENTRY(read_ram_default)
		movb	SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1), %al
		ret

ENTRY(write_ram_default)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		ret

ENTRY(write_ram_lc)
		subw	$0xE000, EffectiveAddr
		movb	%al, SYMBOL_NAME(language_card)(,EffectiveAddr_E,1)
		ret

ENTRY(write_ram_bank0)
		subw	$0xD000, EffectiveAddr
		movb	%al, SYMBOL_NAME(language_bank0)(,EffectiveAddr_E,1)
		ret

ENTRY(write_ram_bank1)
		subw	$0xD000, EffectiveAddr
		movb	%al, SYMBOL_NAME(language_bank1)(,EffectiveAddr_E,1)
		ret

ENTRY(write_ram_text_page0)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	plot_character0
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Hi-res mode?
		jz	plot_block0
		ret

ENTRY(write_ram_text_page1)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	plot_character1
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Hi-res mode?
		jz	plot_block1
		ret

ENTRY(write_ram_text_mixed0)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	plot_character0
		testb	$0xFF, SYMBOL_NAME(vmode_mixed) # Mixed mode?
		jnz	plot_character0
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Not Hi-res mode?
		jz	plot_block0
		ret

ENTRY(write_ram_text_mixed1)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	plot_character1
		testb	$0xFF, SYMBOL_NAME(vmode_mixed) # Mixed mode?
		jnz	plot_character1
		testb	$0xFF, SYMBOL_NAME(vmode_mixed) # Not Hi-res mode?
		jz	plot_block1
		ret

ENTRY(write_ram_hires_page0_even)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	no_hires_page0_even_exit
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Hi-res mode?
		jnz	plot_even_byte0
no_hires_page0_even_exit:
		ret

ENTRY(write_ram_hires_mixed0_even)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	no_hires_mixed0_even_exit
		testb	$0xFF, SYMBOL_NAME(vmode_mixed) # Mixed mode?
		jnz	no_hires_mixed0_even_exit
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Hi-res mode?
		jnz	plot_even_byte0
no_hires_mixed0_even_exit:
		ret

ENTRY(write_ram_hires_page0_odd)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	no_hires_page0_odd_exit
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Hi-res mode?
		jnz	plot_odd_byte0
no_hires_page0_odd_exit:
		ret

ENTRY(write_ram_hires_mixed0_odd)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	no_hires_mixed0_odd_exit
		testb	$0xFF, SYMBOL_NAME(vmode_mixed) # Mixed mode?
		jnz	no_hires_mixed0_odd_exit
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Hi-res mode?
		jnz	plot_odd_byte0
no_hires_mixed0_odd_exit:
		ret

ENTRY(write_ram_hires_page1_even)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	no_hires_page1_even_exit
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Hi-res mode?
		jnz	plot_even_byte1
no_hires_page1_even_exit:
		ret

ENTRY(write_ram_hires_mixed1_even)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	no_hires_mixed1_even_exit
		testb	$0xFF, SYMBOL_NAME(vmode_mixed) # Mixed mode?
		jnz	no_hires_mixed1_even_exit
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Hi-res mode?
		jnz	plot_even_byte1
no_hires_mixed1_even_exit:
		ret

ENTRY(write_ram_hires_page1_odd)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	no_hires_page1_odd_exit
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Hi-res mode?
		jnz	plot_odd_byte1
no_hires_page1_odd_exit:
		ret

ENTRY(write_ram_hires_mixed1_odd)
		movb	%al, SYMBOL_NAME(apple_ii_64k)(,EffectiveAddr_E,1)
		testb	$0xFF, SYMBOL_NAME(vmode_text) # Text mode?
		jnz	no_hires_mixed1_odd_exit
		testb	$0xFF, SYMBOL_NAME(vmode_mixed) # Mixed mode?
		jnz	no_hires_mixed1_odd_exit
		testb	$0xFF, SYMBOL_NAME(vmode_hires) # Hi-res mode?
		jnz	plot_odd_byte1
no_hires_mixed1_odd_exit:
		ret

		.align 4
plot_character0:
		pushal					# Save everything
		xorb	%ah, %ah
		movl	%eax, %esi			# ESI = EAX = Chr code
		subw	$0x400, EffectiveAddr		# Normalize screen addr
		testb	$0xFF, SYMBOL_NAME(vmode_active)		# Current SVGA page
		jz	plot_character0_correct_page
		xorl	%eax, %eax			# Choose page 0
		call	set_page			# Set page 0
plot_character0_correct_page:
		xorl	%eax, %eax
		movb	SYMBOL_NAME(text_page_rows)(,EffectiveAddr_E,1), %al  # Compute row
		leal	(%eax,%eax,4),%eax
		sall	$9, %eax			# * 2560
		addl	$1300, %eax			# Adjust to screen
		xorl	%edx, %edx
		xorl	%ecx, %ecx
		movb	SYMBOL_NAME(text_page_cols)(,EffectiveAddr_E,1), %cl  # Add column
		movb	%cl, %dl
		sall	$3, %edx
		subl	%ecx, %edx
		addl	%edx, %eax
		addl	SYMBOL_NAME(GM), %eax			# Graphic addr
		sall	$6, %esi			# * 64
		addl	$ SYMBOL_NAME(expanded_font), %esi		# Font addr

		movl	(%esi), %ecx			# Draw row 0 of chr
		movl	%ecx, (%eax)
		addl	$4, %esi
		addl	$4, %eax
		movw	(%esi), %cx
		movw	%cx, (%eax)
		addl	$2, %esi
		addl	$2, %eax
		movb	(%esi), %cl
		movb	%cl, (%eax)
		addl	$2, %esi
		addl	$314, %eax			# Go to next row

		movl	(%esi), %ecx			# Draw row 1 of chr
		movl	%ecx, (%eax)
		addl	$4, %esi
		addl	$4, %eax
		movw	(%esi), %cx
		movw	%cx, (%eax)
		addl	$2, %esi
		addl	$2, %eax
		movb	(%esi), %cl
		movb	%cl, (%eax)
		addl	$2, %esi
		addl	$314, %eax			# Go to next row

		movl	(%esi), %ecx			# Draw row 2 of chr
		movl	%ecx, (%eax)
		addl	$4, %esi
		addl	$4, %eax
		movw	(%esi), %cx
		movw	%cx, (%eax)
		addl	$2, %esi
		addl	$2, %eax
		movb	(%esi), %cl
		movb	%cl, (%eax)
		addl	$2, %esi
		addl	$314, %eax			# Go to next row

		movl	(%esi), %ecx			# Draw row 3 of chr
		movl	%ecx, (%eax)
		addl	$4, %esi
		addl	$4, %eax
		movw	(%esi), %cx
		movw	%cx, (%eax)
		addl	$2, %esi
		addl	$2, %eax
		movb	(%esi), %cl
		movb	%cl, (%eax)
		addl	$2, %esi
		addl	$314, %eax			# Go to next row

		movl	(%esi), %ecx			# Draw row 4 of chr
		movl	%ecx, (%eax)
		addl	$4, %esi
		addl	$4, %eax
		movw	(%esi), %cx
		movw	%cx, (%eax)
		addl	$2, %esi
		addl	$2, %eax
		movb	(%esi), %cl
		movb	%cl, (%eax)
		addl	$2, %esi
		addl	$314, %eax			# Go to next row

		movl	(%esi), %ecx			# Draw row 5 of chr
		movl	%ecx, (%eax)
		addl	$4, %esi
		addl	$4, %eax
		movw	(%esi), %cx
		movw	%cx, (%eax)
		addl	$2, %esi
		addl	$2, %eax
		movb	(%esi), %cl
		movb	%cl, (%eax)
		addl	$2, %esi
		addl	$314, %eax			# Go to next row

		movl	(%esi), %ecx			# Draw row 6 of chr
		movl	%ecx, (%eax)
		addl	$4, %esi
		addl	$4, %eax
		movw	(%esi), %cx
		movw	%cx, (%eax)
		addl	$2, %esi
		addl	$2, %eax
		movb	(%esi), %cl
		movb	%cl, (%eax)
		addl	$2, %esi
		addl	$314, %eax			# Go to next row

		movl	(%esi), %ecx			# Draw row 7 of chr
		movl	%ecx, (%eax)
		addl	$4, %esi
		addl	$4, %eax
		movw	(%esi), %cx
		movw	%cx, (%eax)
		addl	$2, %esi
		addl	$2, %eax
		movb	(%esi), %cl
		movb	%cl, (%eax)
		addl	$2, %esi

		popal

		ret

		.align 4
plot_character1:
		pushal
		xorb	%ah, %ah
		movl	%eax, %esi			# ESI = EAX = Chr Code
		subw	$0x800, EffectiveAddr		# Normalize screen addr
		testb	$0xFF, SYMBOL_NAME(vmode_active)		# Current SVGA page
		jnz	plot_character0_correct_page
		movl	$1, %eax
		call	set_page
		jmp	plot_character0_correct_page
plot_block0:
		pushal					# Save everything
		xorb	%ah, %ah
		movl	%eax, %esi			# ESI = EAX = Chr code
		subw	$0x400, EffectiveAddr		# Normalize screen addr
		testb	$0xFF, SYMBOL_NAME(vmode_active)		# Current SVGA page
		jz	plot_block0_correct_page
		xorl	%eax, %eax			# Choose page 0
		call	set_page			# Set page 0
plot_block0_correct_page:
		xorl	%eax, %eax
		movb	SYMBOL_NAME(text_page_rows)(,EffectiveAddr_E,1), %al  # Compute row
		leal	(%eax,%eax,4),%eax
		sall	$9, %eax			# * 2560
		addl	$1300, %eax			# Adjust to screen
		xorl	%edx, %edx
		xorl	%ecx, %ecx
		movb	SYMBOL_NAME(text_page_cols)(,EffectiveAddr_E,1), %cl  # Add column
		movb	%cl, %dl
		sall	$3, %edx
		subl	%ecx, %edx
		addl	%edx, %eax
		addl	SYMBOL_NAME(GM), %eax			# Graphic addr

		movl	%esi, %edx			# Compute color
		andl	$0x0F, %edx
		movb	%dl, %dh
		shll	$16, %edx
		movw	%si, %dx
		andw	$0x0F, %dx
		movb	%dl, %dh

		movl	%edx, (%eax)			# Draw row 0 of blk
		addl	$4, %eax
		movw	%dx, (%eax)
		addl	$2, %eax
		movb	%dl, (%eax)
		addl	$314, %eax			# Go to next row

		movl	%edx, (%eax)			# Draw row 1 of blk
		addl	$4, %eax
		movw	%dx, (%eax)
		addl	$2, %eax
		movb	%dl, (%eax)
		addl	$314, %eax			# Go to next row

		movl	%edx, (%eax)			# Draw row 2 of blk
		addl	$4, %eax
		movw	%dx, (%eax)
		addl	$2, %eax
		movb	%dl, (%eax)
		addl	$314, %eax			# Go to next row

		movl	%edx, (%eax)			# Draw row 3 of blk
		addl	$4, %eax
		movw	%dx, (%eax)
		addl	$2, %eax
		movb	%dl, (%eax)
		addl	$314, %eax			# Go to next row

		movl	%esi, %edx			# Compute color
		andl	$0xF0, %edx
		shrb	$4, %dl
		movb	%dl, %dh
		shll	$16, %edx
		movw	%si, %dx
		andw	$0xF0, %dx
		shrb	$4, %dl
		movb	%dl, %dh

		movl	%edx, (%eax)			# Draw row 4 of blk
		addl	$4, %eax
		movw	%dx, (%eax)
		addl	$2, %eax
		movb	%dl, (%eax)
		addl	$314, %eax			# Go to next row

		movl	%edx, (%eax)			# Draw row 5 of blk
		addl	$4, %eax
		movw	%dx, (%eax)
		addl	$2, %eax
		movb	%dl, (%eax)
		addl	$314, %eax			# Go to next row

		movl	%edx, (%eax)			# Draw row 6 of blk
		addl	$4, %eax
		movw	%dx, (%eax)
		addl	$2, %eax
		movb	%dl, (%eax)
		addl	$314, %eax			# Go to next row

		movl	%edx, (%eax)			# Draw row 7 of blk
		addl	$4, %eax
		movw	%dx, (%eax)
		addl	$2, %eax
		movb	%dl, (%eax)

		popal
		ret

		.align 4
plot_block1:	ret

		.align 4
plot_even_byte0:
		testb	$0xFF, SYMBOL_NAME(vmode_active)		# Current SVGA page
		jz	plot_even_byte0_correct_page
		pushal
		xorl	%eax, %eax			# Choose page 0
		call	set_page			# Set page 0
		popal
plot_even_byte0_correct_page:
		pushl	%eax
		pushl	%ebx
		pushl	%ecx

		xorb	%ah, %ah
		leal	SYMBOL_NAME(expanded_col_highres_even)(,%eax,8), %ebx
		movl	EffectiveAddr_E, %ecx
		subw	$0x2000, EffectiveAddr
		movw	SYMBOL_NAME(hires_page_offset)(,EffectiveAddr_E,2), %ax
		movl	%ecx, EffectiveAddr_E
		addl	SYMBOL_NAME(GM), %eax			# Graphic addr

		movl	(%ebx), %ecx
		movl	%ecx, (%eax)
		addl	$4, %eax
		addl	$4, %ebx
		movw	(%ebx), %cx
		movw	%cx, (%eax)
		addl	$2, %eax
		addl	$2, %ebx
		movb	(%ebx), %cl
		movb	%cl, (%eax)

		popl	%ecx
		popl	%ebx
		popl	%eax

		ret

		.align 4
plot_odd_byte0:
		testb	$0xFF, SYMBOL_NAME(vmode_active)		# Current SVGA page
		jz	plot_odd_byte0_correct_page
		pushal
		xorl	%eax, %eax			# Choose page 0
		call	set_page			# Set page 0
		popal
plot_odd_byte0_correct_page:
		pushl	%eax
		pushl	%ebx
		pushl	%ecx

		xorb	%ah, %ah
		leal	SYMBOL_NAME(expanded_col_highres_odd)(,%eax,8), %ebx
		movl	EffectiveAddr_E, %ecx
		subw	$0x2000, EffectiveAddr
		movw	SYMBOL_NAME(hires_page_offset)(,EffectiveAddr_E,2), %ax
		movl	%ecx, EffectiveAddr_E
		addl	SYMBOL_NAME(GM), %eax			# Graphic addr

		movl	(%ebx), %ecx
		movl	%ecx, (%eax)
		addl	$4, %eax
		addl	$4, %ebx
		movw	(%ebx), %cx
		movw	%cx, (%eax)
		addl	$2, %eax
		addl	$2, %ebx
		movb	(%ebx), %cl
		movb	%cl, (%eax)

		popl	%ecx
		popl	%ebx
		popl	%eax

		ret

		.align 4
plot_even_byte1:
		testb	$0xFF, SYMBOL_NAME(vmode_active)		# Current SVGA page
		jnz	plot_even_byte1_correct_page
		pushal
		movl	$1, %eax			# Choose page 1
		call	set_page			# Set page 0
		popal
plot_even_byte1_correct_page:
		pushl	%eax
		pushl	%ebx
		pushl	%ecx

		xorb	%ah, %ah
		leal	SYMBOL_NAME(expanded_col_highres_even)(,%eax,8), %ebx
		movl	EffectiveAddr_E, %ecx
		subw	$0x4000, EffectiveAddr
		movw	SYMBOL_NAME(hires_page_offset)(,EffectiveAddr_E,2), %ax
		movl	%ecx, EffectiveAddr_E
		addl	SYMBOL_NAME(GM), %eax			# Graphic addr

		movl	(%ebx), %ecx
		movl	%ecx, (%eax)
		addl	$4, %eax
		addl	$4, %ebx
		movw	(%ebx), %cx
		movw	%cx, (%eax)
		addl	$2, %eax
		addl	$2, %ebx
		movb	(%ebx), %cl
		movb	%cl, (%eax)

		popl	%ecx
		popl	%ebx
		popl	%eax

		ret

		.align 4
plot_odd_byte1:
		testb	$0xFF, SYMBOL_NAME(vmode_active)		# Current SVGA page
		jnz	plot_odd_byte1_correct_page
		pushal
		movl	$1, %eax			# Choose page 1
		call	set_page			# Set page 0
		popal
plot_odd_byte1_correct_page:
		pushl	%eax
		pushl	%ebx
		pushl	%ecx

		xorb	%ah, %ah
		leal	SYMBOL_NAME(expanded_col_highres_odd)(,%eax,8), %ebx
		movl	EffectiveAddr_E, %ecx
		subw	$0x4000, EffectiveAddr
		movw	SYMBOL_NAME(hires_page_offset)(,EffectiveAddr_E,2), %ax
		movl	%ecx, EffectiveAddr_E
		addl	SYMBOL_NAME(GM), %eax			# Graphic addr

		movl	(%ebx), %ecx
		movl	%ecx, (%eax)
		addl	$4, %eax
		addl	$4, %ebx
		movw	(%ebx), %cx
		movw	%cx, (%eax)
		addl	$2, %eax
		addl	$2, %ebx
		movb	(%ebx), %cl
		movb	%cl, (%eax)

		popl	%ecx
		popl	%ebx
		popl	%eax

		ret

ENTRY(read_keyboard)
		movb	SYMBOL_NAME(apple_ii_64k)+0xC000, %al
        	ret

ENTRY(read_keyboard_strobe)
		btr	$7, SYMBOL_NAME(apple_ii_64k)+0xC000
		movb	SYMBOL_NAME(apple_ii_64k)+0xC000, %al
		ret

ENTRY(read_random)
		pushal
		call	SYMBOL_NAME(c_read_random)
		popal
		movb	SYMBOL_NAME(random_value), %al
		ret

ENTRY(read_speaker_toggle_pc)
		inb	$0x61, %al
		xorb	$0x2, %al
		outb	%al, $0x61
		ret

ENTRY(read_switch_primary_page)
		testb	$0xFF, SYMBOL_NAME(vmode_page2)
		jnz	_read_switch_primary
		ret
_read_switch_primary:
		movb	$0, SYMBOL_NAME(vmode_page2)
		pushal
		pushl	$0x0
		call	SYMBOL_NAME(c_setscreen)
		addl	$4, %esp
		popal
		ret

ENTRY(read_switch_secondary_page)
		testb	$0xFF, SYMBOL_NAME(vmode_page2)
		jz	_read_switch_secondary
		ret
_read_switch_secondary:
		movb	$1, SYMBOL_NAME(vmode_page2)
		pushal
		pushl	$1
		call	SYMBOL_NAME(c_setscreen)
		addl	$4, %esp
		popal
		ret

ENTRY(read_switch_graphics)
		testb	$0xFF, SYMBOL_NAME(vmode_text)
		jz	switch_graphics_exit
		movb	$0, SYMBOL_NAME(vmode_text)
		call	SYMBOL_NAME(update_video_screen)
switch_graphics_exit:
		ret		

ENTRY(read_switch_text)
		testb	$0xFF, SYMBOL_NAME(vmode_text)
		jnz	switch_text_exit
		movb	$1, SYMBOL_NAME(vmode_text)
		call	SYMBOL_NAME(update_video_screen)
switch_text_exit:
		ret

ENTRY(read_switch_no_mixed)
		testb	$0xFF, SYMBOL_NAME(vmode_mixed)
		jz	switch_no_mixed_exit
		movb	$0, SYMBOL_NAME(vmode_mixed)
		call	SYMBOL_NAME(update_video_screen)
switch_no_mixed_exit:
		ret		

ENTRY(read_switch_mixed)
		testb	$0xFF, SYMBOL_NAME(vmode_mixed)
		jnz	switch_mixed_exit
		movb	$1, SYMBOL_NAME(vmode_mixed)
		call	SYMBOL_NAME(update_video_screen)
switch_mixed_exit:
		ret

ENTRY(read_switch_lores)
		testb	$0xFF, SYMBOL_NAME(vmode_hires)
		jz	switch_lores_exit
		movb	$0, SYMBOL_NAME(vmode_hires)
		call	SYMBOL_NAME(update_video_screen)
switch_lores_exit:
		ret

ENTRY(read_switch_hires)
		testb	$0xFF, SYMBOL_NAME(vmode_hires)
		jnz	switch_hires_exit
		movb	$1, SYMBOL_NAME(vmode_hires)
		call	SYMBOL_NAME(update_video_screen)
switch_hires_exit:
		ret

ENTRY(read_button0)
		movb	SYMBOL_NAME(joy_button0), %al
		ret

ENTRY(read_button1)
		movb	SYMBOL_NAME(joy_button1), %al
		ret

ENTRY(read_button2)
		movb	SYMBOL_NAME(joy_button2), %al
		ret

ENTRY(read_gc0)
		cmpw	$0xFF, joy_trigger0
		je	read_gc0_cont
		incw	joy_trigger0
read_gc0_cont:
		movw	joy_trigger0, %ax
		cmpw	%ax, SYMBOL_NAME(joy_x)
		jge	read_gc0_ff	/* XXXX is this correct? */
		movb	$0, %al
		ret
read_gc0_ff:
		movb	$0xFF, %al
		ret

ENTRY(read_gc1)
		cmpw	$0xFF, joy_trigger1
		je	read_gc1_cont
		incw	joy_trigger1
read_gc1_cont:
		movw	joy_trigger1, %ax
		cmpw	%ax, SYMBOL_NAME(joy_y)
		jge	read_gc1_ff
		movb	$0, %al
		ret
read_gc1_ff:
		movb	$0xFF, %al
		ret

ENTRY(read_gc_strobe)
		movb	$0, joy_trigger0
		movb	$0, joy_trigger1
		movb	$0, joy_trigger2
		movb	$0, joy_trigger3
		ret

ENTRY(lc_c080)
		movb	$0, SYMBOL_NAME(language_card_second)
		cmpb	$0, SYMBOL_NAME(language_card_write)
		jz	lc_c080_cont
		pushal
		LC_Write_Off
		movb	$0, SYMBOL_NAME(language_card_write)
		popal
lc_c080_cont:
		cmpb	$0, SYMBOL_NAME(language_card_read)
		jnz	lc_c080_check_bank
		pushal
		LC_In
		LC_Bank0_In
		movb	$1, SYMBOL_NAME(language_card_read)
		movb	$0, SYMBOL_NAME(language_current_bank)
		popal
		ret
lc_c080_check_bank:
		cmpb	$0, SYMBOL_NAME(language_current_bank)
		jz	lc_c080_bank_ok
		pushal
		LC_Bank1_Out
		LC_Bank0_In
		movb	$0, SYMBOL_NAME(language_current_bank)
		popal
lc_c080_bank_ok:
		ret

ENTRY(lc_c081)
		cmpb	$0, SYMBOL_NAME(language_card_read)
		jz	lc_c081_cont
		movb	$0, SYMBOL_NAME(language_card_read)
		pushal
		LC_Out
		cmpb	$0, SYMBOL_NAME(language_current_bank)
		jnz	lc_c081_bank1
		LC_Bank0_Out
		ROM_In
		popal
		jmp	lc_c081_cont
lc_c081_bank1:
		LC_Bank1_Out
		ROM_In
		popal
lc_c081_cont:
		cmpb	$0, SYMBOL_NAME(language_card_second)
		jz	lc_c081_exit
		cmpb	$2, SYMBOL_NAME(language_card_write)
		je	lc_c081_check_bank
		pushal
		LC_Write_Only
		LC_Write_Only0
		popal
		movb	$0, SYMBOL_NAME(language_current_bank)
		movb	$2, SYMBOL_NAME(language_card_write)
		jmp	lc_c081_exit
lc_c081_check_bank:
		cmpb	$0, SYMBOL_NAME(language_current_bank)
		jz	lc_c081_exit
		pushal
		LC_Write_Only0
		popal
		movb	$0, SYMBOL_NAME(language_current_bank)
lc_c081_exit:
		movb	$1, SYMBOL_NAME(language_card_second)
		ret

ENTRY(lc_c082)
		movb	$0, SYMBOL_NAME(language_card_second)
		cmpb	$0, SYMBOL_NAME(language_card_read)
		jz	lc_c082_cont
		movb	$0, SYMBOL_NAME(language_card_read)
		pushal
		LC_Out
		cmpb	$0, SYMBOL_NAME(language_current_bank)
		jnz	lc_c082_bank1
		LC_Bank0_Out
		ROM_In
		popal
		jmp	lc_c082_cont
lc_c082_bank1:
		LC_Bank1_Out
		ROM_In
		popal
lc_c082_cont:
		cmpb	$0, SYMBOL_NAME(language_card_write)
		jz	lc_c082_exit
		pushal
		LC_Write_Off
		popal
		movb	$0, SYMBOL_NAME(language_card_write)
lc_c082_exit:
		ret

ENTRY(lc_c083)
		cmpb	$0, SYMBOL_NAME(language_card_read)
		jnz	lc_c083_check_bank
		pushal
		LC_In
		LC_Bank0_In
		movb	$1, SYMBOL_NAME(language_card_read)
		movb	$0, SYMBOL_NAME(language_current_bank)
		popal
		jmp	lc_c083_cont
lc_c083_check_bank:
		cmpb	$0, SYMBOL_NAME(language_current_bank)
		jz	lc_c083_cont
		pushal
		LC_Bank1_Out
		LC_Bank0_In
		movb	$0, SYMBOL_NAME(language_current_bank)
		popal
lc_c083_cont:
		cmpb	$0, SYMBOL_NAME(language_card_second)
		jz	lc_c083_exit
		cmpb	$1, SYMBOL_NAME(language_card_write)
		je	lc_c083_exit
		pushal
		LC_Write_On
		movb	$1, SYMBOL_NAME(language_card_write)
		popal
lc_c083_exit:
		movb	$1, SYMBOL_NAME(language_card_second)
		ret

ENTRY(lc_c088)
		movb	$0, SYMBOL_NAME(language_card_second)
		cmpb	$0, SYMBOL_NAME(language_card_write)
		jz	lc_c088_cont
		pushal
		LC_Write_Off
		movb	$0, SYMBOL_NAME(language_card_write)
		popal
lc_c088_cont:
		cmpb	$0, SYMBOL_NAME(language_card_read)
		jnz	lc_c088_check_bank
		pushal
		LC_In
		LC_Bank1_In
		movb	$1, SYMBOL_NAME(language_card_read)
		movb	$1, SYMBOL_NAME(language_current_bank)
		popal
		ret
lc_c088_check_bank:
		cmpb	$1, SYMBOL_NAME(language_current_bank)
		je	lc_c088_bank_ok
		pushal
		LC_Bank0_Out
		LC_Bank1_In
		movb	$1, SYMBOL_NAME(language_current_bank)
		popal
lc_c088_bank_ok:
		ret

ENTRY(lc_c089)
		cmpb	$0, SYMBOL_NAME(language_card_read)
		jz	lc_c089_cont
		movb	$0, SYMBOL_NAME(language_card_read)
		pushal
		LC_Out
		cmpb	$0, SYMBOL_NAME(language_current_bank)
		jnz	lc_c089_bank1
		LC_Bank0_Out
		ROM_In
		popal
		jmp	lc_c089_cont
lc_c089_bank1:
		LC_Bank1_Out
		ROM_In
		popal
lc_c089_cont:
		cmpb	$0, SYMBOL_NAME(language_card_second)
		jz	lc_c089_exit
		cmpb	$2, SYMBOL_NAME(language_card_write)
		je	lc_c089_check_bank
		pushal
		LC_Write_Only
		LC_Write_Only1
		popal
		movb	$1, SYMBOL_NAME(language_current_bank)
		movb	$2, SYMBOL_NAME(language_card_write)
		jmp	lc_c089_exit
lc_c089_check_bank:
		cmpb	$1, SYMBOL_NAME(language_current_bank)
		je	lc_c089_exit
		pushal
		LC_Write_Only1
		popal
		movb	$1, SYMBOL_NAME(language_current_bank)
lc_c089_exit:
		movb	$1, SYMBOL_NAME(language_card_second)
		ret

ENTRY(lc_c08a)
		movb	$0, SYMBOL_NAME(language_card_second)
		cmpb	$0, SYMBOL_NAME(language_card_read)
		jz	lc_c08a_cont
		movb	$0, SYMBOL_NAME(language_card_read)
		pushal
		LC_Out
		cmpb	$0, SYMBOL_NAME(language_current_bank)
		jnz	lc_c08a_bank1
		LC_Bank0_Out
		ROM_In
		popal
		jmp	lc_c08a_cont
lc_c08a_bank1:
		LC_Bank1_Out
		ROM_In
		popal
lc_c08a_cont:
		cmpb	$0, SYMBOL_NAME(language_card_write)
		jz	lc_c08a_exit
		pushal
		LC_Write_Off
		popal
		movb	$0, SYMBOL_NAME(language_card_write)
lc_c08a_exit:
		ret

ENTRY(lc_c08b)
		cmpb	$0, SYMBOL_NAME(language_card_read)
		jnz	lc_c08b_check_bank
		pushal
		LC_In
		LC_Bank1_In
		movb	$1, SYMBOL_NAME(language_card_read)
		movb	$1, SYMBOL_NAME(language_current_bank)
		popal
		jmp	lc_c08b_cont
lc_c08b_check_bank:
		cmpb	$1, SYMBOL_NAME(language_current_bank)
		je	lc_c08b_cont
		pushal
		LC_Bank0_Out
		LC_Bank1_In
		movb	$1, SYMBOL_NAME(language_current_bank)
		popal
lc_c08b_cont:
		cmpb	$0, SYMBOL_NAME(language_card_second)
		jz	lc_c08b_exit
		cmpb	$1, SYMBOL_NAME(language_card_write)
		je	lc_c08b_exit
		pushal
		LC_Write_On
		movb	$1, SYMBOL_NAME(language_card_write)
		popal
lc_c08b_exit:
		movb	$1, SYMBOL_NAME(language_card_second)
		ret


.comm		joy_trigger0, 2
.comm		joy_trigger1, 2
.comm		joy_trigger2, 2
.comm		joy_trigger3, 2

ENTRY(test_read_memory)
		popl	%edi
		call	read_memory
		ret

ENTRY(test_write_memory)
		addl	$4, %esp
		popl	%edi
		popl	%eax
		subl	$12, %esp
		call	write_memory
		ret

debug:
		pushal

		pushl	%edi
		pushl	%esi
		pushl	%edx
		pushl	%ecx
		pushl	%ebx
		pushl	%eax

		call	SYMBOL_NAME(debug)

		addl	$24, %esp

		popal

		ret

z_debug:
		pushal

		pushl	%esp
		pushl	%edi
		pushl	%esi
		pushl	%edx
		pushl	%ecx
		pushl	%ebx
		pushl	%eax

		call	SYMBOL_NAME(z_debug)

		addl	$28, %esp

		popal

		ret

