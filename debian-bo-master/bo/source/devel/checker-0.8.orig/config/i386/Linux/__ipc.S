#include <sys/syscall.h>

#ifdef __ELF__
#define C_NAME(x) x
#else
#define C_NAME(x) _##x
#endif

#ifdef __PIC__
	.text
	.globl C_NAME(chkr__ipc)
	.globl C_NAME(chkr_errno)
	.align 4
C_NAME(chkr__ipc):
  	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
      	call .LL4
.LL4:
 	popl %ebx
	addl $_GLOBAL_OFFSET_TABLE_+[.-.LL4],%ebx
	pushl %ebx
	movl $(SYS_ipc),%eax
	movl 8(%ebp),%ebx
	movl 12(%ebp),%ecx
	movl 16(%ebp),%edx
	movl 20(%ebp),%esi
	movl 24(%ebp),%edi
   	int $0x80
	popl %ebx
	movl %eax,%edx
	test %edx,%edx
	jge .LLexit
	negl %edx
	movl chkr_errno@GOT(%ebx),%eax
	movl %edx,(%eax)
	movl $-1,%eax
.LLexit:
 	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	.type C_NAME(chkr__ipc), @function;
	.size C_NAME(chkr__ipc), . - C_NAME(chkr__ipc)
	ret
#else
	.text;
	.globl C_NAME(chkr__ipc)
	.align 4
C_NAME(chkr__ipc):
  	pushl %ebp
	movl %esp,%ebp
	pushl %edi
	pushl %esi
	pushl %ebx
      	movl $(SYS_ipc), %eax
	movl 8(%ebp),%ebx
	movl 12(%ebp),%ecx
	movl 16(%ebp),%edx
	movl 20(%ebp),%esi
	movl 24(%ebp),%edi
   	int $0x80
	test %eax, %eax
	jge Lexit
	negl %eax
	movl %eax,C_NAME(chkr_errno)
	movl $-1,%eax
Lexit:
 	popl %ebx
	popl %esi
	popl %edi
   	movl %ebp,%esp
	popl %ebp
	ret
#ifdef __ELF__
	.size C_NAME(chkr__ipc), . - C_NAME(chkr__ipc)
	.type C_NAME(chkr__ipc), @function
#endif
#endif
