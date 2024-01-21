#ifdef __ELF__
#define ____sig_restore ___sig_restore
#define ____masksig_restore ___masksig_restore
#define ___sigsetmask __sigsetmask
#endif

.globl ____sig_restore
.globl ____masksig_restore

____sig_restore:
	addl $4,%esp	# signr
	popl %eax
	popl %ecx
	popl %edx
	popf
	ret

____masksig_restore:
#if defined(__PIC__) || defined(__pic__)
	pushl %ebx
	call L3
L3:
	popl %ebx
	addl $_GLOBAL_OFFSET_TABLE_+[.-L3],%ebx
	addl $8,%esp		# signr
	call ___sigsetmask@PLT	# old blocking
	addl $8,%esp
	popl %ebx
	popl %eax
	popl %ecx
	popl %edx
	popf
#else
	addl $4,%esp		# signr
	call ___sigsetmask	# old blocking
	addl $4,%esp
	popl %eax
	popl %ecx
	popl %edx
	popf
#endif
	ret

#ifdef __ELF__
	.type ___masksig_restore,@function
	.type ___sig_restore,@function
#endif
