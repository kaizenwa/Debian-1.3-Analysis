/* Call chkr$main, but set before the rights of argc, argv and envp.  */
/* This was build from main-tramp.c: only a few lines have changed.  */
	.file	"main-tramp.S"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 16
.globl startup_call_main
	.type	 startup_call_main,@function
startup_call_main:
	pushl %ebp
	movl %esp,%ebp
	pushl $3
	pushl $4
	leal 8(%ebp),%edx
	pushl %edx
	call chkr_set_right
	pushl $3
	pushl $4
	leal 12(%ebp),%eax
	pushl %eax
	call chkr_set_right
	pushl $3
	pushl $4
	leal 16(%ebp),%eax
	pushl %eax
	call chkr_set_right
	movl %ebp,%esp
	popl %ebp
	jmp chkr$main
	ret
.Lfe1:
	.size	 startup_call_main,.Lfe1-startup_call_main
	.ident	"GCC: (GNU) 2.7.2"
