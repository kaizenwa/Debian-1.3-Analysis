/* Save the initial execution environment details for the 286
 * executable and invoke the emulator.
 */

		.text
		.align 4
.globl ___START___
.globl _start
___START___:
_start:
		movl %eax,_init_entry
		movl %ebx,_init_cs
		movl %ecx,_init_ds
		movl %edx,_limit_stk
		movl $__boot_crt0,%eax
		jmp *%eax


/* Set the initial execution state and enter the 286 program. */
		.align 4
.globl _x286boot
_x286boot:
		sub  %ax,%ax
		mov  %ax,%bx
		mov  %ax,%cx
		mov  %ax,%dx
		mov  %ax,%bp
		mov  %ax,%si
		mov  %ax,%di
		push %eax
		popfl

		movl _init_stk,%esp
		movl _init_stk,%ebp
		movl _init_ds,%eax
		mov  %ax,%ds
		mov  %ax,%es
		mov  %ax,%ss

		pushl %fs:_init_cs
		pushl %fs:_init_entry
		lret

		.fill 1024,1,0xf4
__boot_crt0:

		.data
		.align 4
		.globl _init_entry
		.globl _init_cs
		.globl _init_ds
		.globl _limit_stk
		.globl _init_stk
_init_entry:	.long 0
_init_cs:	.long 0
_init_ds:	.long 0
_limit_stk:	.long 0
_init_stk:	.long 0
