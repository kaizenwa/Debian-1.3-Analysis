

	.globl	_signal

_signal:	chlvl	#0,#0x30
	jnl	ok	
	jmp	cerror
ok:	ret	#0

