#ifdef __i486__
#define ALIGN_TEXT	.align	16,0x90
#else
#define ALIGN_TEXT	.align	4,0x90
#endif
#define ALIGN_DATA	.align	4


	.section .init
	.globl _init
	.type _init,@function
	ALIGN_TEXT
_init:

	.section .fini
	.globl _fini
	.type _fini,@function
	ALIGN_TEXT
_fini:
