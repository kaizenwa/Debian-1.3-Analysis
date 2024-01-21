
#ifdef __ELF__

.globl __environ
.data

__environ:
	.long 0
.type __environ,@object
/* FIXME - why is this in assembly code, anyway.  On a 64 bit machine, the
.size will be incorrect, but we currently cannot use difference expressions
in .size pseudo-ops. */
.size __environ,4
.weak environ
environ = __environ
#else

.globl ___environ
.data

___environ:
	.long 0

#endif
