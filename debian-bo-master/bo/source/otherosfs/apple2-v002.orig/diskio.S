#define __ASSEMBLY__
#include <linux/linkage.h>

	/* --------------------------------------------------------------------
	    Routine	: disk_read_nop (C0E0, C0E2, C0E4, C0E6)
	   ----------------------------------------------------------------- */

ENTRY(disk_read_nop)
		movzwl	%di, %eax
		andb	$0x6, %al
		shrb	$1, %al		/* AL = phase value */
		xorb	%al, %al	/* @@@ Debug */
		ret

	/* --------------------------------------------------------------------
	    Routine	: disk_read_phase (C0E1, C0E3, C0E5, C0E7)
	   ----------------------------------------------------------------- */

ENTRY(disk_read_phase)
		pushl	%ebx
		pushl	%ecx
		movzwl	%di, %eax
		andb	$0x6, %al
		shrb	$1, %al		/* AL = current phase value */
		movl	SYMBOL_NAME(drive_6), %ecx  /* ECX = drive number */
		movl	SYMBOL_NAME(track_6)(,%ecx,4), %ebx
		andl	$0x3, %ebx
		subb	%bl, %al
		jns	disk_read_phase_over
		addb	$0x4, %al
disk_read_phase_over:
		cmpb	$0x1, %al
		jne	disk_read_phase_over1
		incl	SYMBOL_NAME(track_6)(,%ecx,4)
		jmp	disk_read_phase_exit
disk_read_phase_over1:
		cmpb	$0x3, %al
		jne	disk_read_phase_exit
		decl	SYMBOL_NAME(track_6)(,%ecx,4)
		jns	disk_read_phase_exit
		movl	$0, SYMBOL_NAME(track_6)(,%ecx,4)
disk_read_phase_exit:
		popl	%ecx
		popl	%ebx
		xorb	%al, %al	/* @@@ Debug */
		ret

	/* --------------------------------------------------------------------
	    Routine	: disk_read_motor_off (C0E8)
	   ----------------------------------------------------------------- */

ENTRY(disk_read_motor_off)
		movl	$1, SYMBOL_NAME(motor_6)
		movl	SYMBOL_NAME(drive_6), %eax
		ret

	/* --------------------------------------------------------------------
	    Routine	: disk_read_motor_on (C0E9)
	   ----------------------------------------------------------------- */		

ENTRY(disk_read_motor_on)
		movl	$0, SYMBOL_NAME(motor_6)
		movl	SYMBOL_NAME(drive_6), %eax
		ret

	/* --------------------------------------------------------------------
	    Routine	: disk_read_select_a (C0EA)
	   ----------------------------------------------------------------- */		

ENTRY(disk_read_select_a)
		movl	$0, %eax
		movl	%eax, SYMBOL_NAME(drive_6)
		ret

	/* --------------------------------------------------------------------
	    Routine	: disk_read_select_b (C0EB)
	   ----------------------------------------------------------------- */		

ENTRY(disk_read_select_b)
		movl	$1, %eax
		movl	%eax, SYMBOL_NAME(drive_6)
		ret

	/* --------------------------------------------------------------------
	    Routine	: disk_read_byte (C0EC)
	   ----------------------------------------------------------------- */		

ENTRY(disk_read_byte)
		cmpl	$0, SYMBOL_NAME(ddrw_6)
		jne	disk_read_write_byte
		pushl	%ebx
		pushl	%ecx
		pushl	%edx
		pushl	%esi
		pushl	%edi
		call	SYMBOL_NAME(c_read_byte_6)
		popl	%edi
		popl	%esi
		popl	%edx
		popl	%ecx
		popl	%ebx
		andl	$0xff, %eax
		ret
disk_read_write_byte:
		pusha
		call	SYMBOL_NAME(c_write_byte_6)
		popa
		ret

	/* --------------------------------------------------------------------
	    Routine	: disk_read_latch (C0ED)
	   ----------------------------------------------------------------- */		

ENTRY(disk_read_latch)
		movl	SYMBOL_NAME(drive_6), %eax
		ret

	/* --------------------------------------------------------------------
	    Routine	: disk_read_prepare_in (C0EE)
	   ----------------------------------------------------------------- */		

ENTRY(disk_read_prepare_in)
		movl	$0, SYMBOL_NAME(ddrw_6)
		pushl	%ecx
		movl	SYMBOL_NAME(drive_6), %ecx
		cmpl	$1, SYMBOL_NAME(protected_6)(,%ecx,4)
		je	disk_read_prepare_protected
		movl	$0, %eax
		popl	%ecx
		ret
disk_read_prepare_protected:
		movl	$0x80, %eax
		popl	%ecx
		ret

	/* --------------------------------------------------------------------
	    Routine	: disk_read_prepare_out (C0EF)
	   ----------------------------------------------------------------- */		

ENTRY(disk_read_prepare_out)
		movl	$1, SYMBOL_NAME(ddrw_6)
		movl	SYMBOL_NAME(drive_6), %eax
		ret

	/* --------------------------------------------------------------------
	    Routine	: disk_write_latch (C0ED)
	   ----------------------------------------------------------------- */		

ENTRY(disk_write_latch)
		andl	$0xff, %eax
		movl	%eax, SYMBOL_NAME(disk_byte_6)
		ret


