/* ANSI concatenation macros.  */
#define CONCAT(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b

	.section .text
	.align 	0
LC0:
	.word	0x80000 /* Top of RAM on the PIE board */
LC1:
	.word	__bss_start__
LC2:
	.word	__bss_end__

	.global	_mainCRTStartup
	.global	_start
	.global	start
start:
_start:
_mainCRTStartup:
	ldr	r3, [pc, #LC0 - . - 8]
	mov 	sp, r3
	mov 	fp, #0

	ldr	r0, [pc, #LC1 - . - 8]
	mov	r1, #0
	ldr	r2, [pc, #LC2 - . - 8]
	sub	r2, r2, r0
#ifdef __USER_LABEL_PREFIX__
	bl	CONCAT (__USER_LABEL_PREFIX__, memset)
#else
	bl	_memset
#endif

#ifdef __USER_LABEL_PREFIX__
	bl	CONCAT (__USER_LABEL_PREFIX__, main)
#else
	bl	_main
#endif

#ifdef __USER_LABEL_PREFIX__
	bl	CONCAT (__USER_LABEL_PREFIX__, exit)
#else
	bl	_exit
#endif

#ifdef __pe__
	.section .idata$3
	.long	0,0,0,0,0,0,0,0
#endif
