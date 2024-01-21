/* -------------------------------------------------------------------------
    Macros and equates
   ------------------------------------------------------------------------- */

#define C_Flag		$0x1			/* 6502 Carry	 	   */
#define X_Flag		$0x2			/* 6502 Xtra		   */
#define I_Flag		$0x4			/* 6502 Interrupt disable  */
#define V_Flag		$0x8			/* 6502 Overflow	   */
#define B_Flag		$0x10			/* 6502 Break		   */
#define D_Flag		$0x20			/* 6502 Decimal mode	   */
#define Z_Flag		$0x40			/* 6502 Zero		   */
#define N_Flag		$0x80			/* 6502 Neg		   */

#define NZ_Flag		$0xC0
#define NZC_Flag	$0xC1
#define NVZC_Flag	$0xC9

#define C_Flag_Not	$0xFE
#define Z_Flag_Not      $0xBF
#define NZ_Flag_Not	$0x3F
#define NZC_Flag_Not	$0x3E
#define NVZC_Flag_Not	$0x36
#define NVZ_Flag_Not	$0x37

#define C_Flag_Bit	$8			/* 6502 Carry		   */
#define X_Flag_Bit	$9			/* 6502 Xtra		   */
#define I_Flag_Bit	$10			/* 6502 Interrupt disable  */
#define V_Flag_Bit	$11			/* 6502 Overflow	   */
#define B_Flag_Bit	$12			/* 6502 Break		   */
#define D_Flag_Bit	$13			/* 6502 Decimal mode	   */
#define Z_Flag_Bit	$14			/* 6502 Zero		   */
#define N_Flag_Bit	$15			/* 6502 Neg		   */

#define X_Reg		%bl			/* 6502 X register in %bl  */
#define Y_Reg		%bh			/* 6502 Y register in %bh  */
#define A_Reg		%cl			/* 6502 A register in %cl  */
#define F_Reg		%ch			/* 6502 flags in %ch	   */
#define FF_Reg		%ecx			/* 6502 flags for bt	   */
#define SP_Reg		%edx			/* 6502 Stack pointer	   */
#define SP_Reg_L	%dl			/* 6502 Stack pointer low  */
#define SP_Reg_H	%dh			/* 6502 Stack pointer high */
#define PC_Reg		%si			/* 6502 Program Counter    */
#define PC_Reg_E	%esi			/* 6502 Program Counter    */
#define EffectiveAddr	%di			/* Effective address	   */
#define EffectiveAddr_E	%edi			/* Effective address	   */

/* -------------------------------------------------------------------------
    Video
   ------------------------------------------------------------------------- */

#define Video_Reg	%ebx			/* Video mode register	   */

#define Video_Text_Bit	$24			/* Text mode bit	   */
#define Video_Mixed_Bit $25			/* Mixed mode bit	   */
#define Video_Page2_Bit $26			/* Page2 mode bit	   */
#define Video_Hires_Bit $27			/* Hires mode bit	   */
#define SVGA_Page_Bit	$31			/* SVGA page bit	   */

/* -------------------------------------------------------------------------
    Disk drive
   ------------------------------------------------------------------------- */

#define Disk_Old		$0x1
#define	Disk_Write_Protect	$0x2
#define Disk_Present		$0x4
#define Disk_Motor_On		$0x8
#define Disk_Output_Mode	$0x10
#define Disk_Track_Modified	$0x20
#define Disk_Buffer_Valid	$0x40

#define Disk_Not_Output_Mode	$0xEF
#define Disk_Not_Buffer_Valid	$0xBF
#define Disk_Not_Motor_On	$0xF7

/* -------------------------------------------------------------------------
    CPU (6502)
   ------------------------------------------------------------------------- */

#define GetFromPC_B	movzbl	SYMBOL_NAME(apple_ii_64k)(,%esi,1), %eax;     \
			incw	%si;

#define GetFromPC_W	movzbl	SYMBOL_NAME(apple_ii_64k)(,%esi,1), %eax;     \
			incw	%si;					      \
			movb	SYMBOL_NAME(apple_ii_64k)(,%esi,1), %ah;      \
			incw	%si;

#define Wait		movw	SYMBOL_NAME(apple_speed), %ax;		      \
		   0:							      \
			decw	%ax;					      \
			jnz	0b;

#define Continue	Wait						      \
			testb   $0xFF, SYMBOL_NAME(exception_flag);	      \
			jnz     1f;					      \
			GetFromPC_B		   			      \
			jmp	table_opcodes(,%eax,4);			      \
		   1:   movb	$0, SYMBOL_NAME(exception_flag);	      \
			testb	$0xFF, SYMBOL_NAME(exception_type);	      \
			jnz     2f;					      \
			movl	$0xfffc, %eax;				      \
			movw	SYMBOL_NAME(apple_ii_64k)(,%eax,1), %ax;      \
			movw    %ax, PC_Reg;				      \
			GetFromPC_B					      \
			jmp	table_opcodes(,%eax,4);			      \
		   2:   ret;

#define FlagC		lahf;						      \
  			andb	C_Flag, %ah;			      	      \
  			andb	C_Flag_Not, F_Reg;	      	      	      \
			orb	%ah, F_Reg;

#define FlagZ		lahf;						      \
  			andb	Z_Flag, %ah;			      	      \
  			andb	Z_Flag_Not, F_Reg;	      	      	      \
			orb	%ah, F_Reg;

#define FlagN		lahf;						      \
  			andb	N_Flag, %ah;			      	      \
  			andb	N_Flag_Not, F_Reg;	      	      	      \
			orb	%ah, F_Reg;

#define FlagNZ		lahf;						      \
  			andb	NZ_Flag, %ah;			      	      \
  			andb	NZ_Flag_Not, F_Reg;	      	      	      \
			orb	%ah, F_Reg;

#define FlagNZC		lahf;						      \
			andb	NZC_Flag, %ah;		      		      \
			andb	NZC_Flag_Not, F_Reg;      		      \
			orb	%ah, F_Reg;

#define FlagNVZC	lahf;					      	      \
			seto	%al;					      \
			shlb    $3, %al;				      \
			andb	NZC_Flag, %ah;		      		      \
			andb	NVZC_Flag_Not, F_Reg;      		      \
			orb	%al, %ah;				      \
			orb	%ah, F_Reg;

#define Push(x)		movb	x, SYMBOL_NAME(apple_ii_64k)(,SP_Reg,1);      \
			decb	SP_Reg_L;

#define Pop(x)		incb	SP_Reg_L;				      \
			movb	SYMBOL_NAME(apple_ii_64k)(,SP_Reg,1), x;

#define GetImm		movw	PC_Reg, EffectiveAddr;			      \
			incw	PC_Reg;				      	      \

#define GetAbs		GetFromPC_W;					      \
			movw	%ax, EffectiveAddr;

#define GetZPage	GetFromPC_B;					      \
			movw	%ax, EffectiveAddr;

#define GetZPage_X	GetFromPC_B;					      \
			addb	X_Reg, %al;				      \
			movw	%ax, EffectiveAddr;

#define GetZPage_Y	GetFromPC_B;					      \
			addb	Y_Reg, %al;				      \
			movw	%ax, EffectiveAddr;

#define GetAbs_X	GetFromPC_W;					      \
			addb	X_Reg, %al;				      \
			adcb	$0, %ah;				      \
			movw	%ax, EffectiveAddr;

#define GetAbs_Y	GetFromPC_W;					      \
			addb	Y_Reg, %al;				      \
			adcb	$0, %ah;				      \
			movw	%ax, EffectiveAddr;

#define GetInd		GetFromPC_W;					      \
			movw	SYMBOL_NAME(apple_ii_64k)(,%eax,1), EffectiveAddr;

#define GetIndZPage	GetFromPC_B;					      \
			movw	%ax, %di;				      \
			movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al;      \
			incw	%di;				      	      \
			andw	$0xFF, %di;				      \
			movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %ah;      \
			movw	%ax, EffectiveAddr;

#define GetIndZPage_X	GetFromPC_B;					      \
			addb	X_Reg, %al;				      \
			movw	%ax, %di;				      \
			movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al;      \
			incw	%di;				      	      \
			andw	$0xFF, %di;				      \
			movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %ah;      \
			movw	%ax, EffectiveAddr;

#define GetIndZPage_Y   GetFromPC_B;					      \
			movw	%ax, %di;				      \
			movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %al;      \
			incw	%di;				      	      \
			andw	$0xFF, %di;				      \
			movb	SYMBOL_NAME(apple_ii_64k)(,%edi,1), %ah;      \
			addb	Y_Reg, %al;				      \
			adcb	$0, %ah;				      \
			movw	%ax, EffectiveAddr;

#define DoADC_b		call	read_memory;				      \
			bt	C_Flag_Bit, FF_Reg;			      \
			adcb	%al, A_Reg;				      \
			FlagNVZC

#define	DoADC_d		call	read_memory;				      \
			bt	C_Flag_Bit, FF_Reg;			      \
			adcb	A_Reg, %al;				      \
			daa;						      \
			movb	%al, A_Reg;				      \
			FlagNVZC

#define DoAND		call	read_memory;				      \
			andb	%al, A_Reg;				      \
			FlagNZ

#define DoASL		call	read_memory;				      \
			shlb	$1, %al;				      \
			FlagNZC						      \
			call	write_memory;

#define DoBIT		andb	NVZ_Flag_Not, F_Reg;	      		      \
			call	read_memory;				      \
			testb	%al, A_Reg;				      \
			FlagZ						      \
			movb    %al, %ah;				      \
			andb	$0x40, %al;				      \
			shrb	$3, %al;				      \
			orb	%al, F_Reg;				      \
			andb    $0x80, %ah;				      \
			orb	%ah, F_Reg;

#define DoCMP		call	read_memory;				      \
			cmpb	%al, A_Reg;				      \
			cmc;						      \
			FlagNZC

#define DoCPX		call	read_memory;				      \
			cmpb	%al, X_Reg;				      \
			cmc;						      \
			FlagNZC

#define DoCPY		call	read_memory;				      \
			cmpb	%al, Y_Reg;				      \
			cmc;						      \
			FlagNZC

#define DoDEC		call	read_memory;				      \
			decb	%al;					      \
			FlagNZ						      \
			call	write_memory;

#define DoEOR		call	read_memory;				      \
			xorb	%al, A_Reg;				      \
			FlagNZ

#define DoINC		call	read_memory;				      \
			incb	%al;					      \
			FlagNZ						      \
			call	write_memory;

#define DoJMP		movw	EffectiveAddr, PC_Reg;

#define DoJSR		movw	PC_Reg, %ax;				     \
			decw	%ax;					     \
			Push(%ah)					     \
			Push(%al)					     \
			movw	EffectiveAddr, PC_Reg;

#define DoLDA		call	read_memory;				     \
			movb	%al, A_Reg;				     \
			orb	%al, %al;				     \
			FlagNZ

#define DoLDX		call	read_memory;			  	     \
			movb	%al, X_Reg;				     \
			orb	%al, %al;				     \
			FlagNZ

#define DoLDY		call	read_memory;				     \
			movb	%al, Y_Reg;				     \
			orb	%al, %al;				     \
			FlagNZ

#define DoLSR		call	read_memory;				     \
			shrb	$1, %al;				     \
			FlagNZC						     \
			call	write_memory;

#define DoORA		call	read_memory;				     \
			orb	%al, A_Reg;				     \
			FlagNZ

#define DoROL(L,CONT)	call	read_memory;				     \
			bt	C_Flag_Bit, FF_Reg;			     \
			rclb	$1, %al;				     \
			jc	L;				   	     \
			orb	%al, %al;				     \
			clc;						     \
			FlagNZC						     \
			call	write_memory;				     \
			CONT						     \
	L##:		orb	%al, %al;				     \
			stc;						     \
			FlagNZC						     \
			call	write_memory;				     \
			CONT

#define DoROR(L,CONT)	call	read_memory;				     \
			bt	C_Flag_Bit, FF_Reg;			     \
			rcrb	$1, %al;				     \
			jc	L;					     \
			orb	%al, %al;				     \
			clc;						     \
			FlagNZC						     \
			call	write_memory;				     \
			CONT						     \
	L##:		orb	%al, %al;				     \
			stc;						     \
			FlagNZC						     \
			call	write_memory;				     \
			CONT

#define DoSBC_b		call	read_memory;				     \
			bt	C_Flag_Bit, FF_Reg;			     \
			cmc;						     \
			sbbb	%al, A_Reg;				     \
			cmc;						     \
			FlagNVZC

#define DoSBC_d		call	read_memory;				     \
			bt	C_Flag_Bit, FF_Reg;			     \
			cmc;						     \
			xchgb	A_Reg, %al;				     \
			sbbb	A_Reg, %al;				     \
			das;						     \
			movb	%al, A_Reg;				     \
			cmc;						     \
			FlagNVZC

#define DoSTA		movb	A_Reg, %al;				     \
			call	write_memory;

#define DoSTX		movb	X_Reg, %al;				     \
			call	write_memory;

#define DoSTY		movb	Y_Reg, %al;				     \
			call	write_memory;

/* -------------------------------------------------------------------------
    Undocumented 6502 (Illegal instructions)
   ------------------------------------------------------------------------- */

        /* AAX = A AND X -> M */
#define DoAAX		movb	A_Reg, %al;				     \
			andb	X_Reg, %al;				     \
			FlagNZ						     \
			call	write_memory;

	/* AMA = ORA 238, AND M, TAX */
#define DoAMA		orb	$238, A_Reg;				     \
			call	read_memory;				     \
			andb	%al, A_Reg;				     \
			movb	A_Reg, X_Reg;				     \
			FlagNZ

	/* ANA = AND M, Carry = BIT 7 */
#define DoANA(CONT)	call	read_memory;				     \
			andb	%al, A_Reg;				     \
			js	1f;					     \
			clc;						     \
			FlagNZC						     \
			CONT						     \
		1:	stc;						     \
			FlagNZC						     \
			CONT

	/* ANB = same as ANA */
#define DoANB(CONT)	call	read_memory;				     \
			andb	%al, A_Reg;				     \
			js	1f;					     \
			clc;						     \
			FlagNZC						     \
			CONT						     \
		1:	stc;						     \
			FlagNZC						     \
			CONT

	/* AXM = (A AND X) - M -> X */
#define DoAXM		call	read_memory;				     \
			andb	A_Reg, X_Reg;				     \
			bt	C_Flag_Bit, FF_Reg;			     \
			cmc;						     \
			sbbb	%al, X_Reg;				     \
			cmc;						     \
			FlagNVZC

	/* AXS = (A AND X) -> S, A AND X AND 17 -> M */
#define DoAXS		movb	A_Reg, SP_Reg_L;			     \
			andb	X_Reg, SP_Reg_L;			     \
			movb	SP_Reg_L, %al;				     \
			andb	$17, %al;				     \
			FlagNZ
			call	write_memory;

	/* DCP = DEC M, CMP M */
#define	DoDCP		call	read_memory;				     \
			decb	%al;					     \
			cmpb	%al, A_Reg;				     \
			cmc;						     \
			FlagNZC						     \
			call	write_memory;

	/* ISB = INC M, SBC M */
#define DoISB_b		call	read_memory;				     \
			incb	%al;					     \
			call	write_memory;				     \
			bt	C_Flag_Bit, FF_Reg;			     \
			cmc;						     \
			sbbb	%al, A_Reg;				     \
			cmc;						     \
			FlagNVZC

#define DoISB_d		call	read_memory;				     \
			incb	%al;					     \
			call	write_memory;				     \
			bt	C_Flag_Bit, FF_Reg;			     \
			cmc;						     \
			xchgb	A_Reg, %al;				     \
			sbbb	A_Reg, %al;				     \
			das;						     \
			movb	%al, A_Reg;				     \
			cmc;						     \
			FlagNVZC

	/* LAN = ROL M, AND M */
#define DoLAN(CONT)	call	read_memory;				     \
			bt	C_Flag_Bit, FF_Reg;			     \
			rclb	$1, %al;				     \
			jc	1f;					     \
			call	write_memory;				     \
			andb	%al, A_Reg;				     \
			clc;						     \
			FlagNZC						     \
			CONT						     \
		   1:	call	write_memory;				     \
			andb	%al, A_Reg;				     \
			stc;						     \
			FlagNZC						     \
			CONT

	/* LAS = LDA M, TAX, TXS */
#define DoLAS		call	read_memory;				     \
			movb	%al, A_Reg;				     \
			movb	%al, X_Reg;				     \
			movb	%al, SP_Reg_L;				     \
			orb	%al, %al;				     \
			FlagNZ

	/* LAX = LDA M, TAX */
#define DoLAX		call	read_memory;				     \
			movb	%al, A_Reg;				     \
			movb	%al, X_Reg;				     \
			orb	%al, %al;				     \
			FlagNZ

	/* LOR = ASL M, ORA M */
#define DoLOR		call	read_memory;				     \
			shlb	$1, %al;				     \
			FlagC						     \
			call	write_memory;				     \
			orb	%al, A_Reg;				     \
			FlagNZ

	/* RAD = ROR M, ADC M */
#define DoRAD_b		call	read_memory;				     \
			bt	C_Flag_Bit, FF_Reg;			     \
			rcrb	$1, %al;				     \
			adcb	%al, A_Reg;				     \
			pushl	%eax;					     \
			FlagNVZC					     \
			popl	%eax;					     \
			call	write_memory;

#define DoRAD_d		call	read_memory;				     \
			bt	C_Flag_Bit, FF_Reg;			     \
			rcrb	$1, %al;				     \
			pushfl;						     \
			call	write_memory;				     \
			popfl;						     \
			adcb	A_Reg, %al;				     \
			daa;						     \
			movb	%al, A_Reg;				     \
			FlagNVZC

	/* RAM = AND M, LSR A */
#define DoRAM		call	read_memory;				     \
			andb	%al, A_Reg;				     \
			shrb	$1, A_Reg;				     \
			FlagNZC

	/* RBM = same as RAM */
#define DoRBM		DoRAM

	/* REO = LSR M, EOR M */
#define DoREO		call	read_memory;				     \
			shrb	$1, %al;				     \
			xorb	%al, A_Reg;				     \
			FlagNZC						     \
			call	write_memory;

	/* DoZBC = same as SBC */
#define DoZBC_b		DoSBC_b
#define DoZBC_d		DoSBC_d

	/* TEA = (A AND X AND (OP+2)+1) -> M */
#define	DoTEA		pushl	EffectiveAddr_E;			     \
			movw	PC_Reg, EffectiveAddr;			     \
			decw	EffectiveAddr;			     	     \
			call	read_memory;				     \
			popl	EffectiveAddr_E;			     \
			incb	%al;					     \
			andb	A_Reg, %al;				     \
			andb	X_Reg, %al;				     \
			FlagNZ						     \
			call	write_memory;

	/* TEX = (X AND (OP+2)+1) -> M */
#define DoTEX		pushl	EffectiveAddr_E;			     \
			movw	PC_Reg, EffectiveAddr;			     \
			decw	EffectiveAddr;			     	     \
			call	read_memory;				     \
			popl	EffectiveAddr_E;			     \
			incb	%al;					     \
			andb	X_Reg, %al;				     \
			FlagNZ						     \
			call	write_memory;

	/* TEY = (Y AND 1) -> M */
#define DoTEY		movb	Y_Reg, %al;				     \
			andb	$1, %al;				     \
			FlagNZ						     \
			call	write_memory;

	/* XMA = (X AND M) AND (A OR 238) -> A */
#define DoXMA
			call	read_memory;				     \
			andb	X_Reg, %al;				     \
			orb	$238, A_Reg;				     \
			andb	%al, A_Reg;				     \
			FlagNZ

#define LC_Write_On	leal	SYMBOL_NAME(write_ram_default), %eax;	     \
			movl	$ SYMBOL_NAME(table_write_memory)+0x34000, %edi;     \
			movl	$0x3000, %ecx;				     \
			rep;						     \
			stosl;

#define LC_Write_Off	leal	SYMBOL_NAME(ram_nop), %eax;		     \
			movl	$ SYMBOL_NAME(table_write_memory)+0x34000, %edi;     \
			movl	$0x3000, %ecx;				     \
			rep;						     \
			stosl;

#define LC_Write_Only	leal	SYMBOL_NAME(write_ram_lc), %eax;	     \
			movl	$ SYMBOL_NAME(table_write_memory)+0x38000, %edi;     \
			movl	$0x2000, %ecx;				     \
			rep;						     \
			stosl;

#define LC_Write_Only0	leal	SYMBOL_NAME(write_ram_bank0), %eax;	     \
			movl	$ SYMBOL_NAME(table_write_memory)+0x34000, %edi;     \
			movl	$0x1000, %ecx;				     \
			rep;						     \
			stosl;

#define LC_Write_Only1	leal	SYMBOL_NAME(write_ram_bank1), %eax;	     \
			movl	$ SYMBOL_NAME(table_write_memory)+0x34000, %edi;     \
			movl	$0x1000, %ecx;				     \
			rep;						     \
			stosl;

#define ROM_In		movl	$ SYMBOL_NAME(apple_ii_64k)+0xD000, %edi;    \
			leal	SYMBOL_NAME(apple_ii_rom), %esi;	     \
			movl	$0xC00, %ecx;				     \
			rep;						     \
			movsl;

#define LC_Out		movl	$ SYMBOL_NAME(apple_ii_64k)+0xE000, %esi;    \
			leal	SYMBOL_NAME(language_card), %edi;	     \
			movl	$0x800, %ecx;				     \
			rep;						     \
			movsl;

#define LC_In		movl	$ SYMBOL_NAME(apple_ii_64k)+0xE000, %edi;    \
			leal	SYMBOL_NAME(language_card), %esi;	     \
			movl	$0x800, %ecx;				     \
			rep;						     \
			movsl;

#define LC_Bank0_Out	movl	$ SYMBOL_NAME(apple_ii_64k)+0xD000, %esi;    \
			leal	SYMBOL_NAME(language_bank0), %edi;	     \
			movl	$0x400, %ecx;				     \
			rep;						     \
			movsl;

#define LC_Bank0_In	movl	$ SYMBOL_NAME(apple_ii_64k)+0xD000, %edi;    \
			leal	SYMBOL_NAME(language_bank0), %esi;	     \
			movl	$0x400, %ecx;				     \
			rep;						     \
			movsl;

#define LC_Bank1_Out	movl	$ SYMBOL_NAME(apple_ii_64k)+0xD000, %esi;    \
			leal	SYMBOL_NAME(language_bank1), %edi;	     \
			movl	$0x400, %ecx;				     \
			rep;						     \
			movsl;

#define LC_Bank1_In	movl	$ SYMBOL_NAME(apple_ii_64k)+0xD000, %edi;    \
			leal	SYMBOL_NAME(language_bank1), %esi;	     \
			movl	$0x400, %ecx;				     \
			rep;						     \
			movsl;
