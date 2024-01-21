.file	"get_regs.c"
gcc2_compiled.:
.section	".text"

	.align 4
	.global get_g7
	.type	 get_g7,#function
	.proc	016
get_g7:
	retl
	mov %g7,%o0
.LLfe1:
	.size	 get_g7,.LLfe1-get_g7
	.align 4
	.global get_i6
	.type	 get_i6,#function
	.proc	016
get_i6:
	retl
	mov %i6,%o0
.LLfe2:
	.size	 get_i6,.LLfe2-get_i6
	.ident	"GCC: (GNU) 2.7.2"
