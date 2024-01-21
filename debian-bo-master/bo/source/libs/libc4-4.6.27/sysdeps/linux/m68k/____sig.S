.globl ____sig_restore
.globl ____masksig_restore

____sig_restore:
	addql #4,sp    | signr
/*	  popl %eax
	popl %ecx
	popl %edx
	popfl*/
	rts

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
	popfl
#else
	addql #4,sp	       | signr
	jbsr   ___sigsetmask   | old blocking
	addql #4,sp
/*	  popl %eax
	popl %ecx
	popl %edx
	popfl */
#endif
	rts
